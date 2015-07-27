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
        INPUT_FIFO_LENGTH : integer := 32;
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
        
        s_axis_tdata_1 : in std_logic_vector(DATA_WIDTH-1 downto 0);
        s_axis_tvalid_1 : in std_logic;
        s_axis_tready_1 : out std_logic;
        s_axis_tlast_1 : in std_logic;
                
        s_axis_tdata_2 : in std_logic_vector(DATA_WIDTH-1 downto 0);
        s_axis_tvalid_2 : in std_logic;
        s_axis_tready_2 : out std_logic;
        s_axis_tlast_2 : in std_logic;
        
        s_axis_tdata_3 : in std_logic_vector(DATA_WIDTH-1 downto 0);
        s_axis_tvalid_3 : in std_logic;
        s_axis_tready_3 : out std_logic;
        s_axis_tlast_3 : in std_logic;
        
        m_axis_tdata : out std_logic_vector(DATA_WIDTH-1 downto 0);
        m_axis_tvalid : out std_logic;
        m_axis_tready : in std_logic;
        m_axis_tlast : out std_logic;
        
        instruction : out std_logic_vector(CODE_BITS+3*DATA_WIDTH downto 0);
        valid : out std_logic;
        ready : in std_logic;
        
        rows_total : in std_logic_vector(DATA_WIDTH-1 downto 0);
        
        code : out std_logic_vector(CODE_BITS downto 0);
        
        data_return : in std_logic_vector(DATA_WIDTH-1 downto 0);
        data_valid : in std_logic;
        data_empty : in std_logic;
		  
		  NNZ_PKG2PE : out std_logic_vector(31 downto 0);
		  STM_STATUS : out std_logic_vector(31 downto 0)
    );
end packager;

architecture Behavioral of packager is

-- sinais da fifo de X
signal x_valid, x_empty, x_full, x_read : std_logic := '0';
signal x_in, x_out : std_logic_vector(DATA_WIDTH downto 0) := (others => '0');
signal data_count_x : std_logic_vector(7 downto 0);

-- sinais da fifo de VAL
signal val_valid, val_empty, val_full, val_read : std_logic := '0';
signal val_out : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
signal data_count_val : std_logic_vector(7 downto 0);

-- sinais da fifo de IND
signal ind_valid, ind_empty, ind_full, ind_read : std_logic := '0';
signal ind_out : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
signal data_count_ind : std_logic_vector(7 downto 0);

-- sinais da fifo de ROW_PTR
signal ptr_valid, ptr_empty, ptr_full, ptr_read : std_logic := '0';
signal ptr_out, ptr_reg : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
signal data_count_ptr : std_logic_vector(7 downto 0);

signal counter_curr, counter_next : std_logic_vector(ceil_log2(SIZE)+CODE_BITS downto 0) := (others => '0');  
signal length_curr, length_next, columns_sent_curr, columns_sent_next : integer := 0;
--signal total_column_number : integer := -1;
signal reset_processors, invalid_column : std_logic := '0';

signal delayed_output_reg, delayed_output : std_logic_vector(ceil_log2(SIZE)+CODE_BITS+DATA_WIDTH downto 0) := (others => '1');

signal test_counter_curr, test_counter_next : integer := 0;

-- maquina de estados
type STATES is (reset, init_read, init_save, check_X_PTR, read_X_PTR, check_VAL_IND, read_VAL_IND, get_results, check_results);
signal curr_state, next_state : STATES := reset; 

signal pkg_stm_code : std_logic_vector(3 downto 0);

begin
	 
    -- instancia da fifo de X
    fifo_x: simpleFIFO
    generic map(
        DATA_WIDTH => DATA_WIDTH+1,
        FIFO_LENGTH => INPUT_FIFO_LENGTH,
        LOG2_FIFO_LENGTH => ceil_log2(INPUT_FIFO_LENGTH)
    )
    PORT MAP(
        CLK => clk,
        RST => rst,
        
        DIN => x_in,
        WR_EN => s_axis_tvalid_0,
        RD_EN => x_read,
        DOUT => x_out,
        FIFO_FULL => x_full,
        FIFO_VALID => x_valid,
        FIFO_EMPTY => x_empty,
        DATA_COUNT => data_count_x
    ); 
             
    -- instancia da fifo de VAL
    fifo_val: simpleFIFO
    generic map(
        DATA_WIDTH => DATA_WIDTH,
        FIFO_LENGTH => INPUT_FIFO_LENGTH,
        LOG2_FIFO_LENGTH => ceil_log2(INPUT_FIFO_LENGTH)
    )
    PORT MAP(
        CLK => clk,
        RST => rst,
        
        DIN => s_axis_tdata_1,
        WR_EN => s_axis_tvalid_1,
        RD_EN => val_read,
        DOUT => val_out,
        FIFO_FULL => val_full,
        FIFO_VALID => val_valid,
        FIFO_EMPTY => val_empty,
        DATA_COUNT => data_count_val
    );
        
    -- instancia da fifo de IND
    fifo_ind: simpleFIFO 
    generic map(
        DATA_WIDTH => DATA_WIDTH,
        FIFO_LENGTH => INPUT_FIFO_LENGTH,
        LOG2_FIFO_LENGTH => ceil_log2(INPUT_FIFO_LENGTH)
    )
    PORT MAP(
        CLK => clk,
        RST => rst,
        
        DIN => s_axis_tdata_2,
        WR_EN => s_axis_tvalid_2,
        RD_EN => ind_read,
        DOUT => ind_out,
        FIFO_FULL => ind_full,
        FIFO_VALID => ind_valid,
        FIFO_EMPTY => ind_empty,
        DATA_COUNT => data_count_ind
    );
    
    -- instancia da fifo de IND
    fifo_ptr: simpleFIFO 
    generic map(
        DATA_WIDTH => DATA_WIDTH,
        FIFO_LENGTH => INPUT_FIFO_LENGTH,
        LOG2_FIFO_LENGTH => ceil_log2(INPUT_FIFO_LENGTH)
    )
    PORT MAP(
        CLK => clk,
        RST => rst,
        
        DIN => s_axis_tdata_3,
        WR_EN => s_axis_tvalid_3,
        RD_EN => ptr_read,
        DOUT => ptr_out,
        FIFO_FULL => ptr_full,
        FIFO_VALID => ptr_valid,
        FIFO_EMPTY => ptr_empty,
        DATA_COUNT => data_count_ptr
    ); 
    
    -- finite state machine
    process(clk, rst) is
    begin
        if (rst = '1') then
            curr_state <= reset;
				counter_curr <= (others => '0');
            length_curr <= 0;
            columns_sent_curr <= 0;
				test_counter_curr <= 0;
				delayed_output_reg <= (others => '1');
        elsif (clk'event and clk = '1') then
            curr_state <= next_state;
            counter_curr <= counter_next;
            length_curr <= length_next;
            columns_sent_curr <= columns_sent_next;
				test_counter_curr <= test_counter_next;
				delayed_output_reg <= delayed_output;
        end if;
    end process;
    
    -- next_state logic
    process(curr_state, x_empty, val_empty, ind_empty, ptr_empty, x_valid, val_valid, ind_valid, ptr_valid, data_empty, ready, length_curr, counter_curr, x_out, ptr_out, val_out, ind_out, m_axis_tready) is
    begin
        -- defaults
		  pkg_stm_code <= "0000";
        reset_processors <= '0';
        x_read <= '0';
        val_read <= '0';
        ind_read <= '0';
        ptr_read <= '0';
        instruction <= (others => '0');
        valid <= '0';
        next_state <= curr_state;
        counter_next <= counter_curr;
        length_next <= length_curr;
        columns_sent_next <= columns_sent_curr;
		test_counter_next <= test_counter_curr;
    
        case curr_state is
            
            -- reset state
            when reset =>
					 pkg_stm_code <= "0001";
                reset_processors <= '1';
                code(0) <= '1';
                next_state <= init_read;
                counter_next <= (others => '0');
            -- initial read of ROW_PTR
            when init_read =>
					pkg_stm_code <= "0010";
                reset_processors <= '1';
                code(0) <= '1';
--					counter_next <= (others => '0');
					length_next <= 0;
					columns_sent_next <= 0;
--					test_counter_next <= 0;
					invalid_column <= '0';
                if ptr_empty = '0' then
                    ptr_read <= '1';
                    next_state <= init_save;
                end if;
				
			when init_save =>
				pkg_stm_code <= "0011";
				if ptr_valid = '1' then
					ptr_reg <= ptr_out;
					next_state <= check_X_PTR;
				end if;
                
            -- check X and ROW_PTR fifos to have values, and receiving the Y values
            when check_X_PTR =>
					pkg_stm_code <= "0100";
                if x_out(x_out'high) = '1' and x_empty = '1' and ptr_empty = '1' then --and val_empty = '1' and ind_empty = '1'
                    columns_sent_next <= 0;
                    next_state <= check_results;
                elsif ptr_empty = '0' and x_empty = '0' and ready = '1' then
                    ptr_read <= '1';
                    x_read <= '1';
                    columns_sent_next <= columns_sent_curr + 1;
                    next_state <= read_X_PTR;
                end if;
                counter_next <= (others => '0');
            -- gets X and ROW_PTR values
            when read_X_PTR =>
				pkg_stm_code <= "0101";
					if x_valid = '1' and ptr_valid = '1' then
                    if ptr_out /= ptr_reg then
                        if to_integer(unsigned(x_out(DATA_WIDTH-1 downto 0))) = 0 then
                            invalid_column <= '1';
                        end if;
                        -- instruction(DATA_WIDTH-1 downto 0) <= x_out(DATA_WIDTH-1 downto 0);
                        length_next <= to_integer(unsigned(ptr_out)) - to_integer(unsigned(ptr_reg));   
--                        if val_empty = '0' and ind_empty = '0' and ready = '1' then
--                            ind_read <= '1';
--                            val_read <= '1';
--							test_counter_next <= test_counter_curr + 1;
--                            length_next <= to_integer(unsigned(ptr_out)) - to_integer(unsigned(ptr_reg)) - 1;   
--                            next_state <= read_VAL_IND;
--                        else
                            next_state <= check_VAL_IND;
--                        end if;
                    else
                        next_state <= check_X_PTR;
                    end if;
                end if;
                
            -- check the VAL and IND fifos to have values
            when check_VAL_IND =>
					pkg_stm_code <= "0110";
                if val_empty = '0' and ind_empty = '0' and ready = '1' then
                    ind_read <= '1';
                    val_read <= '1';
					test_counter_next <= test_counter_curr + 1;
                    length_next <= length_curr - 1;   
                    next_state <= read_VAL_IND;
                end if;
                
            -- get VAL and IND values
            when read_VAL_IND => 
					pkg_stm_code <= "0111";
                if val_valid = '1' and ind_valid = '1' then
                    -- PID = LSBs do indice
                    instruction(instruction'high downto 3*DATA_WIDTH) <= ('0' & ind_out(CODE_BITS-1 downto 0))+1;
                    -- envia o X
                    instruction(3*DATA_WIDTH-1 downto 2*DATA_WIDTH) <= x_out(DATA_WIDTH-1 downto 0);
                    -- envia o VAL
                    instruction(2*DATA_WIDTH-1 downto DATA_WIDTH) <= val_out;
                    -- normaliza o IND
                    instruction(DATA_WIDTH-CODE_BITS-1 downto 0) <= ind_out(DATA_WIDTH-1 downto CODE_BITS);
                    if invalid_column = '0' then
                        valid <= '1';
                    end if;
                    if length_curr = 0 then
                        invalid_column <= '0';
                        ptr_reg <= ptr_out;
--                        if ptr_empty = '0' and x_empty = '0' and ready = '1' and x_out(x_out'high) = '0' then
--                            ptr_read <= '1';
--                            x_read <= '1';
--                            columns_sent_next <= columns_sent_curr + 1;
--                            next_state <= read_X_PTR;
--                        else
                            next_state <= check_X_PTR;
--                        end if;
                    else
--                        if val_empty = '0' and ind_empty = '0' and ready = '1' then
--                            ind_read <= '1';
--                            val_read <= '1';
--							test_counter_next <= test_counter_curr + 1;
--                            length_next <= length_curr - 1;   
--                        else
                            next_state <= check_VAL_IND; 
--                        end if;
                    end if;
                end if;
                                    
                -- recebe os resultados dos processadores
                -- recebe os resultados dos processadores
                when get_results =>
					pkg_stm_code <= "1000";
                    -- PID
                    instruction(3*DATA_WIDTH+CODE_BITS downto 3*DATA_WIDTH) <= ('0' & counter_curr(CODE_BITS-1 downto 0))+1;
                    -- Indice
                    instruction(ceil_log2(SIZE)-1 downto 0) <= counter_curr(counter_curr'high-1 downto CODE_BITS);
					valid <= '1';   
					counter_next <= counter_curr + 1;
					next_state <= check_results;
                            
                when check_results =>
						pkg_stm_code <= "1001";
                  code(0) <= '0';
						if counter_curr = rows_total then
	--						counter_next <= (others => '0');
							next_state <= reset;
						elsif data_empty = '0' and m_axis_tready = '1' and to_integer(unsigned(counter_curr)) < P*SIZE then
									 next_state <= get_results;
						end if;
                    
        end case;
    end process;
    
    code(CODE_BITS downto 1) <= (others => '0');
    
    x_in <= s_axis_tlast_0 & s_axis_tdata_0;
    
    s_axis_tready_0 <= '1' when (((INPUT_FIFO_LENGTH - to_integer(unsigned(data_count_x)))>2) and rst = '0') else '0';
    s_axis_tready_1 <= '1' when (((INPUT_FIFO_LENGTH - to_integer(unsigned(data_count_val)))>2) and rst = '0') else '0';
    s_axis_tready_2 <= '1' when (((INPUT_FIFO_LENGTH - to_integer(unsigned(data_count_ind)))>2) and rst = '0') else '0';
    s_axis_tready_3 <= '1' when (((INPUT_FIFO_LENGTH - to_integer(unsigned(data_count_ptr)))>2) and rst = '0') else '0';
    
    m_axis_tdata <= delayed_output_reg(delayed_output_reg'high downto ceil_log2(SIZE)+CODE_BITS+1) when delayed_output_reg(ceil_log2(SIZE)+CODE_BITS downto 0) = counter_curr else data_return;
    m_axis_tvalid <= '1' when delayed_output_reg(ceil_log2(SIZE)+CODE_BITS downto 0) = counter_curr and curr_state = get_results else data_valid;
    m_axis_tlast <= '1' when (data_valid = '1' and counter_curr = rows_total) else '0';
    
    delayed_output <= data_return & counter_curr when data_valid = '1' and m_axis_tready = '0';
    
    reset_processor_system <= reset_processors;
	 
	 NNZ_PKG2PE <= std_logic_vector(to_unsigned(test_counter_curr, 32));
	 STM_STATUS <= counter_curr & "0" & rows_total(15 downto 0) & pkg_stm_code;
	 
end Behavioral;