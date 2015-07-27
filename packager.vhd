	library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

library mylibs_v1_00_a;
use mylibs_v1_00_a.float_pkg.all;
use mylibs_v1_00_a.SoC_Pkg.all;

entity packager is
    generic(
        P : integer := 2;
        CODE_BITS : integer := 1; -- valor equivalente a log2(P)
        SIZE : integer := 1024;
        INPUT_FIFO_LENGTH : integer := 256;
        DATA_WIDTH : integer := 32
    );
    Port(
        clk : in std_logic;
        rst : in std_logic;
        reset_processor_system : out std_logic;
                        
        s_axis_tdata_0 : in std_logic_vector(DATA_WIDTH-1 downto 0);
        s_axis_tvalid_0 : in std_logic;
        s_axis_tready_0 : out std_logic;
        s_axis_tlast_0 : in std_logic;
        
--        s_axis_tdata_1 : in std_logic_vector(DATA_WIDTH-1 downto 0);
--        s_axis_tvalid_1 : in std_logic;
--        s_axis_tready_1 : out std_logic;
--        s_axis_tlast_1 : in std_logic;
                
        s_axis_tdata_2 : in std_logic_vector(DATA_WIDTH-1 downto 0);
        s_axis_tvalid_2 : in std_logic;
        s_axis_tready_2 : out std_logic;
        s_axis_tlast_2 : in std_logic;
--        
--        s_axis_tdata_3 : in std_logic_vector(DATA_WIDTH-1 downto 0);
--        s_axis_tvalid_3 : in std_logic;
--        s_axis_tready_3 : out std_logic;
--        s_axis_tlast_3 : in std_logic;
        
        m_axis_tdata : out std_logic_vector(DATA_WIDTH-1 downto 0);
        m_axis_tvalid : out std_logic;
        m_axis_tready : in std_logic;
        m_axis_tlast : out std_logic;
        
        instruction : out std_logic_vector(CODE_BITS+3*DATA_WIDTH downto 0);
        valid : out std_logic;
        ready : in std_logic;
        
        rows_total : in std_logic_vector(DATA_WIDTH-1 downto 0);
        
        code : out std_logic_vector(CODE_BITS downto 0);
		idle : out std_logic_vector(DATA_WIDTH-1 downto 0);
		fifos_state : out std_logic_vector(3 downto 0);
		contador_estados : out VEC1;
        
        data_return : in std_logic_vector(DATA_WIDTH-1 downto 0);
        data_valid : in std_logic;
        packager_finished : out std_logic;
        processors_finished : in std_logic;
		  
        NNZ_PKG2PE : out std_logic_vector(31 downto 0);
        STM_STATUS : out std_logic_vector(31 downto 0)
    );
end packager;

architecture Behavioral of packager is


component xil_fifo33_256 IS
--component fifo_generator_0 IS
  PORT (
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    din : IN STD_LOGIC_VECTOR(32 DOWNTO 0);
    wr_en : IN STD_LOGIC;
    rd_en : IN STD_LOGIC;
    dout : OUT STD_LOGIC_VECTOR(32 DOWNTO 0);
    full : OUT STD_LOGIC;
    empty : OUT STD_LOGIC;
    valid : OUT STD_LOGIC;
    data_count : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
  );
END component;

-- sinais da fifo de X
signal x_valid, x_empty, x_full, x_read : std_logic;
signal x_in, x_out : std_logic_vector(DATA_WIDTH downto 0);
signal data_count_x : std_logic_vector(7 downto 0);

-- sinais da fifo de VAL
--signal val_valid, val_empty, val_full, val_read : std_logic;
--signal val_in, val_out : std_logic_vector(DATA_WIDTH downto 0);
--signal data_count_val : std_logic_vector(7 downto 0);

-- sinais da fifo de IND
signal ind_valid, ind_empty, ind_full, ind_read : std_logic;
signal ind_in, ind_out : std_logic_vector(DATA_WIDTH downto 0);
signal data_count_ind : std_logic_vector(7 downto 0);

-- sinais da fifo de ROW_PTR
--signal ptr_valid, ptr_empty, ptr_full, ptr_read : std_logic;
--signal ptr_in, ptr_out : std_logic_vector(DATA_WIDTH downto 0);
--signal data_count_ptr : std_logic_vector(7 downto 0);

-- contadores
signal ptr_reg_curr, ptr_reg_next : std_logic_vector(DATA_WIDTH-1 downto 0);
signal counter_curr, counter_next : std_logic_vector(ceil_log2(SIZE)+CODE_BITS downto 0); 
signal length_curr, length_next : integer;
signal idle_curr, idle_next : std_logic_vector(DATA_WIDTH-1 downto 0);
signal test_counter_curr, test_counter_next : integer;

-- maquina de estados
type STATES is (reset, receive_results, init_read, init_save, check_X_PTR, read_X_PTR, check_VAL_IND, read_VAL_IND, get_results);
signal curr_state, next_state : STATES := reset; 

signal pkg_stm_code : std_logic_vector(3 downto 0);

signal contador_estados_curr, contador_estados_next : VEC1;

begin
	 
    -- instancia da fifo de X
    fifo_x: xil_fifo33_256
    --fifo_x: fifo_generator_0
    PORT MAP(
        CLK => clk,
        RST => rst,
        
        DIN => x_in,
        WR_EN => s_axis_tvalid_0,
        RD_EN => x_read,
        DOUT => x_out,
        FULL => x_full,
        VALID => x_valid,
        EMPTY => x_empty,
        DATA_COUNT => data_count_x
    ); 
             
--    -- instancia da fifo de VAL
--    fifo_val: xil_fifo33_256
--    --fifo_val: fifo_generator_0
--    PORT MAP(
--        CLK => clk,
--        RST => rst,
--        
--        DIN => val_in,
--        WR_EN => s_axis_tvalid_1,
--        RD_EN => val_read,
--        DOUT => val_out,
--        FULL => val_full,
--        VALID => val_valid,
--        EMPTY => val_empty,
--        DATA_COUNT => data_count_val
--    );
        
    -- instancia da fifo de IND
    fifo_ind: xil_fifo33_256
    --fifo_ind: fifo_generator_0
    PORT MAP(
        CLK => clk,
        RST => rst,
        
        DIN => ind_in,
        WR_EN => s_axis_tvalid_2,
        RD_EN => ind_read,
        DOUT => ind_out,
        FULL => ind_full,
        VALID => ind_valid,
        EMPTY => ind_empty,
        DATA_COUNT => data_count_ind
    );
    
    -- instancia da fifo de IND
--    fifo_ptr: xil_fifo33_256 
--    --fifo_ptr: fifo_generator_0
--    PORT MAP(
--        CLK => clk,
--        RST => rst,
--        
--        DIN => ptr_in,
--        WR_EN => s_axis_tvalid_3,
--        RD_EN => ptr_read,
--        DOUT => ptr_out,
--        FULL => ptr_full,
--        VALID => ptr_valid,
--        EMPTY => ptr_empty,
--        DATA_COUNT => data_count_ptr
--    ); 
    
    -- finite state machine
    process(clk, rst) is
    begin
        if (rst = '1') then
            curr_state <= reset;
            counter_curr <= (others => '0');
            length_curr <= 0;
            test_counter_curr <= 0;
            ptr_reg_curr <= (others => '0');
			
        elsif (clk'event and clk = '1') then
            curr_state <= next_state;
            counter_curr <= counter_next;
            length_curr <= length_next;
            test_counter_curr <= test_counter_next;
            ptr_reg_curr <= ptr_reg_next;
			idle_curr <= idle_next;
			contador_estados_curr <= contador_estados_next;
				
        end if;
    end process;
    
	process(curr_state, data_valid, x_empty, ind_empty, x_valid, ind_valid, processors_finished, ready, length_curr, counter_curr, x_out, ind_out, m_axis_tready, test_counter_curr, ptr_reg_curr) is
	begin
		-- defaults
		pkg_stm_code <= "0000";
		reset_processor_system <= '0';
		x_read <= '0';
		--val_read <= '0';
		ind_read <= '0';
		--ptr_read <= '0';
		instruction <= (others => '0');
		valid <= '0';
		packager_finished <= '0';
		next_state <= curr_state;
		counter_next <= counter_curr;
		length_next <= length_curr;
		test_counter_next <= test_counter_curr;
		ptr_reg_next <= ptr_reg_curr;
		idle_next <= idle_curr;
		contador_estados_next <= contador_estados_curr;

		case curr_state is
		
			when reset =>
				pkg_stm_code <= "0001";
				contador_estados_next(0) <= contador_estados_curr(0) + 1;
				reset_processor_system <= '1';
				length_next <= 0;
				test_counter_next <= 0;
				ptr_reg_next <= (others => '0');
				counter_next <= (others => '0');
				next_state <= init_read;
			
			when init_read =>
				contador_estados_next(1) <= contador_estados_curr(1) + 1;
				pkg_stm_code <= "0010";
--				if ptr_empty = '0' then
--					ptr_read <= '1';
--					next_state <= init_save;
--				end if;
				if ind_empty = '0' then
					ind_read <= '1';
					next_state <= init_save;
				end if;

			when init_save =>
				pkg_stm_code <= "0011";
				contador_estados_next(2) <= contador_estados_curr(2) + 1;
--				if ptr_valid = '1' then
--					ptr_reg_next <= ptr_out(DATA_WIDTH-1 downto 0);
--					next_state <= check_X_PTR;
--				end if;
				if ind_valid = '1' then
					ptr_reg_next <= ind_out(DATA_WIDTH-1 downto 0);
					next_state <= check_X_PTR;
				end if;
				 
			when check_X_PTR =>
				pkg_stm_code <= "0100";
				contador_estados_next(3) <= contador_estados_curr(3) + 1;
				counter_next <= (others => '0');
				idle_next <= idle_curr + 1;
--				if x_out(x_out'high) = '1' and val_out(val_out'high) = '1' and ind_out(ind_out'high) = '1' and ptr_out(ptr_out'high) = '1' then
--					next_state <= get_results;
--				elsif ptr_empty = '0' and x_empty = '0' and ready = '1' then
--					ptr_read <= '1';
--					x_read <= '1';
--					next_state <= read_X_PTR;
--				end if;
				if x_out(x_out'high) = '1' and ind_out(ind_out'high) = '1' then
					next_state <= get_results;
				elsif ind_empty = '0' and x_empty = '0' and ready = '1' then
					ind_read <= '1';
					x_read <= '1';
					next_state <= read_X_PTR;
				end if;
			
			when read_X_PTR =>
				pkg_stm_code <= "0101";
				contador_estados_next(4) <= contador_estados_curr(4) + 1;
--				if x_valid = '1' and ptr_valid = '1' then
--					if ptr_reg_curr /= ptr_out(DATA_WIDTH-1 downto 0) then
--						length_next <= to_integer(unsigned(ptr_out(DATA_WIDTH-1 downto 0))) - to_integer(unsigned(ptr_reg_curr));  
--						next_state <= check_VAL_IND;
--					else
--						next_state <= check_X_PTR;
--					end if;
--				end if;
				if x_valid = '1' and ind_valid = '1' then
					if ptr_reg_curr /= ind_out(DATA_WIDTH-1 downto 0) then
						length_next <= to_integer(unsigned(ind_out(DATA_WIDTH-1 downto 0))) - to_integer(unsigned(ptr_reg_curr));  
						next_state <= check_VAL_IND;
					else
						next_state <= check_X_PTR;
					end if;
				end if;
				 
			when check_VAL_IND =>
				pkg_stm_code <= "0110";
				contador_estados_next(5) <= contador_estados_curr(5) + 1;
				idle_next <= idle_curr + 1;
--				if val_empty = '0' and ind_empty = '0' and ready = '1' then
--					ind_read <= '1';
--					val_read <= '1';
--					test_counter_next <= test_counter_curr + 1;
--					length_next <= length_curr - 1;   
--					next_state <= read_VAL_IND;
--				end if;
				if x_empty = '0' and ind_empty = '0' and ready = '1' then
					ind_read <= '1';
					x_read <= '1';
					test_counter_next <= test_counter_curr + 1;
					length_next <= length_curr - 1;   
					next_state <= read_VAL_IND;
				end if;
				 
			when read_VAL_IND => 
				pkg_stm_code <= "0111";
				contador_estados_next(6) <= contador_estados_curr(6) + 1;
--				if val_valid = '1' and ind_valid = '1' then
--					instruction(instruction'high downto 3*DATA_WIDTH) <= ('0' & ind_out(CODE_BITS-1 downto 0))+1;
--					instruction(3*DATA_WIDTH-1 downto 2*DATA_WIDTH) <= x_out(DATA_WIDTH-1 downto 0);
--					instruction(2*DATA_WIDTH-1 downto DATA_WIDTH) <= val_out(DATA_WIDTH-1 downto 0);
--					instruction(DATA_WIDTH-CODE_BITS-1 downto 0) <= ind_out(DATA_WIDTH-1 downto CODE_BITS);
--					if x_out(DATA_WIDTH-1 downto 0) /= 0 then
--						valid <= '1';
--					end if;
--					if length_curr = 0 then
--						ptr_reg_next <= ptr_out(DATA_WIDTH-1 downto 0);
--						next_state <= check_X_PTR;	
--					else
--						next_state <= check_VAL_IND; 
--					end if;
--				end if;
				if x_valid = '1' and ind_valid = '1' then
					instruction(instruction'high downto 3*DATA_WIDTH) <= ('0' & ind_out(CODE_BITS-1 downto 0))+1;
					instruction(3*DATA_WIDTH-1 downto 2*DATA_WIDTH) <= x_out(DATA_WIDTH-1 downto 0);
					instruction(2*DATA_WIDTH-1 downto DATA_WIDTH) <= x_out(DATA_WIDTH-1 downto 0);
					instruction(DATA_WIDTH-CODE_BITS-1 downto 0) <= ind_out(DATA_WIDTH-1 downto CODE_BITS);
					if x_out(DATA_WIDTH-1 downto 0) /= 0 then
						valid <= '1';
					end if;
					if length_curr = 0 then
						ptr_reg_next <= ind_out(DATA_WIDTH-1 downto 0);
						next_state <= check_X_PTR;	
					else
						next_state <= check_VAL_IND; 
					end if;
				end if;
								 
			when get_results =>
				pkg_stm_code <= "1000";
				contador_estados_next(7) <= contador_estados_curr(7) + 1;
				packager_finished <= '1';
				if counter_curr = rows_total then
					next_state <= reset;
				elsif processors_finished = '1' and m_axis_tready = '1' then
					instruction(3*DATA_WIDTH+CODE_BITS downto 3*DATA_WIDTH) <= ('0' & counter_curr(CODE_BITS-1 downto 0))+1;
					instruction(ceil_log2(SIZE)-1 downto 0) <= counter_curr(counter_curr'high-1 downto CODE_BITS);
					valid <= '1';   
					counter_next <= counter_curr + 1;
					next_state <= receive_results;
				end if;
				
			when receive_results => 
				pkg_stm_code <= "1001";
				contador_estados_next(8) <= contador_estados_curr(8) + 1;
				packager_finished <= '1';   
				next_state <= get_results;
				
		end case;		  
	end process;

	code(CODE_BITS downto 1) <= (others => '0');
	code(0) <= '1';

	x_in <= s_axis_tlast_0 & s_axis_tdata_0;
	--val_in <= s_axis_tlast_1 & s_axis_tdata_1;
   ind_in <= s_axis_tlast_2 & s_axis_tdata_2;
	--ptr_in <= s_axis_tlast_3 & s_axis_tdata_3;

	s_axis_tready_0 <= not x_full when (INPUT_FIFO_LENGTH - to_integer(unsigned(data_count_x)))>2 else '0';
	--s_axis_tready_1 <= not val_full when (INPUT_FIFO_LENGTH - to_integer(unsigned(data_count_val)))>2 else '0';
	s_axis_tready_2 <= not ind_full when (INPUT_FIFO_LENGTH - to_integer(unsigned(data_count_ind)))>2 else '0';
	--s_axis_tready_3 <= not ptr_full when (INPUT_FIFO_LENGTH - to_integer(unsigned(data_count_ptr)))>2 else '0';

	m_axis_tdata <= data_return;
	m_axis_tvalid <= data_valid;
	m_axis_tlast <= data_valid when counter_curr = rows_total else '0';

	idle <= idle_curr;
	--fifos_state <= x_empty & val_empty & ind_empty & ptr_empty;
	fifos_state <= x_empty & '1' & ind_empty & '1';
	contador_estados <= contador_estados_curr;

	NNZ_PKG2PE <= std_logic_vector(to_unsigned(test_counter_curr, 32));
	STM_STATUS <= counter_curr & "00" & x"000" & pkg_stm_code;
	 
end Behavioral;