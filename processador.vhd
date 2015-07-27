library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.MATH_REAL.ALL;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

library mylibs_v1_00_a;
use mylibs_v1_00_a.float_pkg.all;
use mylibs_v1_00_a.SoC_Pkg.all;

entity processor is
generic(
        P : integer := 4;
        LOG_P : integer := 2;
        DATA_WIDTH : INTEGER := 32;
        FIFO_LENGTH : integer := 32;
        MEM_SIZE : integer := 1024
);
Port (         
        clk : in std_logic;
        rst : in std_logic;
               
        instruction_in : in std_logic_vector(LOG_P+3*DATA_WIDTH downto 0);
        instruction_valid_in : in std_logic;
        
        instruction_stop_in : in std_logic;
        instruction_stop_out : out std_logic;
        
        code_in : in std_logic_vector(LOG_P downto 0);
        code_out : out std_logic_vector(LOG_P downto 0);
        
        packager_finished : in std_logic;
			state : out std_logic_vector(7 downto 0);
			collisions : out std_logic_vector(DATA_WIDTH-1 downto 0);
		-- saida : out processor_output;
        
        datain : in std_logic_vector(DATA_WIDTH-1 downto 0);
        validin : in std_logic;
        finishedin : in std_logic;
        
        dataout : out std_logic_vector(DATA_WIDTH-1 downto 0);
        validout : out std_logic;
        finishedout	: out std_logic
);
end processor;

architecture Behavioral of processor is
    
 component xil_fifo96_32 IS
PORT (
 clk : IN STD_LOGIC;
 rst : IN STD_LOGIC;
 din : IN STD_LOGIC_VECTOR(95 DOWNTO 0);
 wr_en : IN STD_LOGIC;
 rd_en : IN STD_LOGIC;
 dout : OUT STD_LOGIC_VECTOR(95 DOWNTO 0);
 full : OUT STD_LOGIC;
 empty : OUT STD_LOGIC;
 valid : OUT STD_LOGIC;
 data_count : OUT STD_LOGIC_VECTOR(4 DOWNTO 0)
);
END component;
	 
    signal zero : std_logic;
    signal one : std_logic;
    
    -- buffer fifo
    signal values_out : std_logic_vector(3*DATA_WIDTH-1 downto 0);
    signal values_w, values_r, values_full, values_valid, values_empty : std_logic;
    signal values_count : std_logic_vector(ceil_log2(FIFO_LENGTH)-1 downto 0);
    
    -- mem block
    signal mem_write : std_logic;
    signal mem_addr_a, mem_addr_b : std_logic_vector(ceil_log2(MEM_SIZE)-1 downto 0);
    signal data_in, mem_out : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    -- FMA
    signal INIT : std_logic_vector(0 downto 0);
    signal fma_result : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal fma_done : std_logic;
    
    -- delay block
    signal delay_out, delay_in : std_logic_vector(DATA_WIDTH-1 downto 0);
	signal flag_out_curr, flag_out_next : std_logic;
        
    -- mem de dirty bits
    type db_value is array (natural(ceil(real(MEM_SIZE)/real(1024)))-1 downto 0) of std_logic_vector(0 downto 0);
    signal write_b : db_value;
    signal dirtybit_value : db_value;
    signal done_fma : db_value;
    
    -- verificacao de termino da matriz
    signal pops_curr, pops_next : integer;
    signal pops_validated_next, pops_validated_curr : integer;

    -- maquina de estados
    type STATES is (state_0, state_1, state_2, check_state, get_results1, get_results2);
    signal curr_state, next_state : STATES; 
    
begin
    
    -- instancia da fifo de buffer
    fifo_buffer: xil_fifo96_32 
    PORT MAP(
        CLK => clk,
        RST => rst,
        
        DIN => instruction_in(3*DATA_WIDTH-1 downto 0),
        WR_EN => values_w,
        RD_EN => values_r,
        DOUT => values_out,
        FULL => values_full,
        VALID => values_valid,
        EMPTY => values_empty,
        DATA_COUNT => values_count
    );
    
    mem: MEMC
    generic map(
        DATA_WIDTH => DATA_WIDTH,
        SIZE => MEM_SIZE
    )
    port map(
       clk => clk,
       wea => mem_write,
       addra => mem_addr_a(ceil_log2(MEM_SIZE)-1 downto 0),
       dina => data_in,
       addrb => mem_addr_b(ceil_log2(MEM_SIZE)-1 downto 0),
       doutb => mem_out
    );
        
    FMA: sg_fma_wrapper
      GENERIC MAP(
        OPX_WIDTH => DATA_WIDTH,
        OPY_WIDTH => DATA_WIDTH, 
        OPW_WIDTH => DATA_WIDTH, 
        MUL_RES_WIDTH =>  DATA_WIDTH+DATA_WIDTH/2,
        OP_RES_WIDTH =>  DATA_WIDTH
      )
      PORT MAP(
        CLK => clk,
        RST => rst,
        CE => one,
        INIT_CALC => INIT(0),
        WRITE2FILE => zero,
        X => values_out(3*DATA_WIDTH-1 downto 2*DATA_WIDTH),
        Y => values_out(2*DATA_WIDTH-1 downto DATA_WIDTH),
        W => mem_out,
        RES => fma_result,
        DONE => fma_done
      );
     
    -- dirty bit memory vector   
    dirty_bit_array: for i in 0 to natural(ceil(real(MEM_SIZE)/real(1024)))-1 generate
    begin
        dirty_bits : blk_mem_gen_0
        port map(
           clka => clk,
           wea => done_fma(i),
           addra => delay_out(9 downto 0),
           dina => "0",
           douta => open,
           clkb => clk,
           web => write_b(i),
           addrb => values_out(9 downto 0),
           dinb => "1",
           doutb => dirtybit_value(i)
        );
    end generate;
	
    delay: bus_delay_block
    generic map(
        bits => 32
    )
    port map(
        CLK => clk,
        RESET => rst,
        A => delay_in,
        A_DELAYED => delay_out
    );
    
    -- state machine
    process(clk, rst) is
    begin        
        if (rst = '1') then
            curr_state <= check_state;
            flag_out_curr <= '0';
			pops_curr <= 0;
			pops_validated_curr <= 0;
			
        elsif (clk'event and clk = '1') then    
            curr_state <= next_state;
            pops_curr <= pops_next;
			flag_out_curr <= flag_out_next;
			pops_validated_curr <= pops_validated_next;
			
        end if;
    end process;
	
	-- next state logic
    process(curr_state, fma_done, values_empty, values_valid, code_in, instruction_valid_in, dirtybit_value, validin, datain, fma_result, flag_out_curr, pops_curr, pops_validated_curr, delay_out, values_out, instruction_in, mem_out, mem_addr_b) is
    begin
		-- defaults
		values_r <= '0';
        done_fma <= (others => (others => '0'));
		pops_validated_next <= pops_validated_curr;
		mem_write <= '0';
		INIT(0) <= '0';
		next_state <= curr_state;
		pops_next <= pops_curr;
		flag_out_next <= flag_out_curr;
		delay_in <= (others => '0');
		write_b <= (others => "0");
		data_in <= fma_result;
		dataout <= datain;
		validout <= validin;
		
		if fma_done = '1' then
			mem_write <= '1';
			mem_addr_a <= delay_out(ceil_log2(MEM_SIZE)-1 downto 0);
            done_fma(to_integer(unsigned(delay_out(DATA_WIDTH-1 downto 10)))) <= "1";
            pops_validated_next <= pops_validated_curr + 1;
		end if;
		
		case curr_state is
				
			-- verificação do estado da fifo
			when check_state =>
				if values_empty = '0' then
					values_r <= '1';
					next_state <= state_0;
				elsif (packager_finished = '1' and pops_curr = pops_validated_curr and values_empty = '1') then
					-- flag para sinalizar fim da computação
					flag_out_next <= '1';
					next_state <= get_results1;
				end if;
				
			-- get values to compute
			when state_0 =>
				if values_valid = '1' then
					mem_addr_b <= values_out(ceil_log2(MEM_SIZE)-1 downto 0);
					pops_next <= pops_curr + 1;
					next_state <= state_1;
				end if;
			
			-- espera que colisao dissipe
			when state_1 =>
				if dirtybit_value(to_integer(unsigned(values_out(DATA_WIDTH-1 downto 10)))) = "0" then
					next_state <= state_2;
				end if;
				
			-- nao houve colisao, prossegue
			when state_2 => 
				INIT(0) <= '1';
				delay_in <= values_out(DATA_WIDTH-1 downto 0);
				write_b(to_integer(unsigned(values_out(DATA_WIDTH-1 downto 10)))) <= "1";
				next_state <= check_state;
				
			-- envio de valores
			when get_results1 =>
				if instruction_valid_in = '1' and instruction_in(LOG_P+3*DATA_WIDTH downto 3*DATA_WIDTH) = code_in then
					mem_addr_b <= instruction_in(ceil_log2(MEM_SIZE)-1 downto 0);
					next_state <= get_results2;
				end if;
			
			when get_results2 =>
				data_in <= (others => '0');
				mem_write <= '1';
				dataout <= mem_out;
				validout <= '1';
				mem_addr_a <= mem_addr_b;
				next_state <= get_results1;
				
		end case;
	end process;
    
    values_w <= '1' when (instruction_in(LOG_P+3*DATA_WIDTH downto 3*DATA_WIDTH) = code_in and instruction_valid_in = '1' and flag_out_curr = '0') else '0';
	
	instruction_stop_out <= values_full when (instruction_stop_in = '1' OR (FIFO_LENGTH - to_integer(unsigned(values_count)))<3) else '0';
	
    code_out <= code_in+1;
    
    finishedout <= flag_out_curr and finishedin;
	
	zero <= '0';
	one <= '1';
        
end Behavioral;