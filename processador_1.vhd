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
        
        datain : in std_logic_vector(DATA_WIDTH-1 downto 0);
        validin : in std_logic;
        emptyin : in std_logic;
        
        dataout : out std_logic_vector(DATA_WIDTH-1 downto 0);
        validout : out std_logic;
        emptyout : out std_logic
);
end processor;

architecture Behavioral of processor is
    
    signal trigger, zero, temp_end: std_logic := '0';
    signal one : std_logic := '1';
    signal PID, zeros : std_logic_vector(LOG_P downto 0) := (others => '0');
    
    -- buffer fifo
    signal values_out : std_logic_vector(3*DATA_WIDTH-1 downto 0) := (others => '0');
    signal values_w, values_r, values_full, values_valid, values_empty : std_logic := '0';
    signal values_count : std_logic_vector(ceil_log2(FIFO_LENGTH)-1 downto 0) := (others => '0');
    
    -- mem block
    signal mem_write : std_logic := '0';
    signal mem_addr_a1, mem_addr_a2, mem_addr_b1 : std_logic_vector(ceil_log2(MEM_SIZE)-1 downto 0) := (others => '0');
    signal data_in : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
    
    -- FMA
    signal ADD : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
    signal mem_value : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
    signal INIT : std_logic_vector(0 downto 0) := "0";
    signal fma_result : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
    signal fma_done : std_logic := '0';
    
    -- delay block
    signal delay_out, delay_in : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
	 signal trigger_out_curr, trigger_out_next : std_logic := '0';
    
begin
    
    -- instancia da fifo de buffer
    fifo_buffer: simpleFIFO 
    generic map(
        DATA_WIDTH => 3*DATA_WIDTH,
        FIFO_LENGTH => FIFO_LENGTH,
        LOG2_FIFO_LENGTH => ceil_log2(FIFO_LENGTH)
    )
    PORT MAP(
        CLK => clk,
        RST => rst,
        
        DIN => instruction_in(3*DATA_WIDTH-1 downto 0),
        WR_EN => values_w,
        RD_EN => values_r,
        DOUT => values_out,
        FIFO_FULL => values_full,
        FIFO_VALID => values_valid,
        FIFO_EMPTY => values_empty,
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
       addra => mem_addr_a2(ceil_log2(MEM_SIZE)-1 downto 0),
       dina => data_in,
       addrb => mem_addr_b1(ceil_log2(MEM_SIZE)-1 downto 0),
       doutb => ADD
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
        W => mem_value,
        RES => fma_result,
        DONE => fma_done
      );

    FSM: fsm_pe
    generic map(
        DATA_WIDTH => DATA_WIDTH,
        SIZE => MEM_SIZE,
        CODE_SIZE => LOG_P
    )
    Port map (
        clk => clk,
        rst => rst,
        
        read_fifo => values_r,
        empty_fifo => values_empty,
        valid_fifo => values_valid,
        
        ind_escrita_in => delay_out,
        ind_leitura_in => values_out(DATA_WIDTH-1 downto 0),
        fma_done => fma_done,
        fma_ready => INIT(0),
        
        the_end => temp_end,
        done => code_in
    );
    
    delay: bus_delay_block
    generic map(
        bits => 32,
        delay => 8
    )
    port map(
        CLK => clk,
        RESET => rst,
        CE => '1',
        A => delay_in,
        A_DELAYED => delay_out
    );
    
    process(clk, rst) is
    begin
        if (rst = '1') then
            trigger_out_curr <= '0';
            PID <= code_in;
				
        elsif (clk'event and clk = '1') then
            -- default
				trigger_out_curr <= trigger_out_next;
            
            -- caso em que pede valores finais 
            if (instruction_in(LOG_P+3*DATA_WIDTH downto 3*DATA_WIDTH) = PID and instruction_valid_in = '1' and temp_end = '1') then
                trigger_out_next <= '1';
                mem_addr_a1 <= instruction_in(ceil_log2(MEM_SIZE)-1 downto 0);
				 else
					trigger_out_next <= '0';
            end if;
        end if;
    end process;
    
    values_w <= '1' when (instruction_in(LOG_P+3*DATA_WIDTH downto 3*DATA_WIDTH) = PID and instruction_valid_in = '1' and temp_end = '0') else '0';
    
	instruction_stop_out <= '1' when (instruction_stop_in = '1' OR (FIFO_LENGTH - to_integer(unsigned(values_count)))<3 OR rst = '1') else '0';
    
    code_out <= code_in+1 when (code_in /= 0) else (others => '0');
    
    delay_in <= values_out(DATA_WIDTH-1 downto 0) when INIT(0) = '1';
    
    data_in <= fma_result when temp_end = '0' else (others => '1'); --'0'
    mem_write <= fma_done when temp_end = '0' else trigger_out_curr;
    mem_addr_a2 <= delay_out(ceil_log2(MEM_SIZE)-1 downto 0) when temp_end = '0' else mem_addr_a1;
    mem_addr_b1 <= instruction_in(ceil_log2(MEM_SIZE)-1 downto 0) when temp_end = '1' and instruction_valid_in = '1' else values_out(ceil_log2(MEM_SIZE)-1 downto 0);
    
    mem_value <= ADD;
    dataout <= ADD when trigger_out_curr = '1' else datain;
    validout <= trigger_out_curr OR validin when rst = '0' else '0';
    emptyout <= (not temp_end) OR emptyin;
        
end Behavioral;