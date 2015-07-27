library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use IEEE.MATH_REAL.ALL;
use ieee.std_logic_unsigned.all;

library mylibs_v1_00_a;
use mylibs_v1_00_a.float_pkg.all;
use mylibs_v1_00_a.SoC_Pkg.all;

entity fsm_pe is
generic(
    DATA_WIDTH : integer := 32;
	SIZE : integer := 1024;
	CODE_SIZE : integer := 1
);
Port (
    clk : in std_logic;
    rst : in std_logic;
    
    read_fifo : out std_logic := '0';
    empty_fifo : in std_logic;
    valid_fifo : in std_logic;
    
    ind_escrita_in : in std_logic_vector(DATA_WIDTH-1 downto 0);
    ind_leitura_in : in std_logic_vector(DATA_WIDTH-1 downto 0);
    fma_done : in std_logic;
    fma_ready : out std_logic := '0';
    
    the_end : out std_logic := '0';
    done : in std_logic_vector(CODE_SIZE downto 0)
);
end fsm_pe;

architecture Behavioral of fsm_pe is

    -- maquina de estados
    type STATES is (reset, state_0, state_1, state_2, check_state);
    signal curr_state, next_state : STATES; 
        
    -- mem de dirty bits
    type db_value is array (natural(ceil(real(SIZE)/real(1024)))-1 downto 0) of std_logic_vector(0 downto 0);
    signal write_b : db_value := (others => (others => '0'));
    signal dirtybit_value : db_value := (others => (others => '0'));
    signal done_fma : db_value := (others => (others => '0'));
    
    -- verificacao de termino da matriz
    signal pops_curr, pops_next : integer := 0;
    signal pops_validated_next, pops_validated_curr : integer := 0;
    
begin
     
    -- dirty bit memory vector   
    dirty_bit_array: for i in 0 to natural(ceil(real(SIZE)/real(1024)))-1 generate
    begin
        dirty_bits : blk_mem_gen_0
        port map(
           clka => clk,
           wea => done_fma(i),
           addra => ind_escrita_in(9 downto 0),
           dina => "0",
           douta => open,
           clkb => clk,
           web => write_b(i),
           addrb => ind_leitura_in(9 downto 0),
           dinb => "1",
           doutb => dirtybit_value(i)
        );
    end generate;
    
    -- state machine
    process(clk, rst) is
    begin        
        if (rst = '1') then
            curr_state <= reset;
        elsif (clk'event and clk = '1') then    
            curr_state <= next_state;
            pops_curr <= pops_next;
            pops_validated_curr <= pops_validated_next;
        end if;
    end process;
    
    -- processo à parte para set a 0 do dirty bit e contabilizar ouputs
    process(fma_done, rst)
    begin
        -- defaults
        done_fma <= (others => (others => '0'));
		  pops_validated_next <= pops_validated_curr;
        
		  if rst = '1' then
				pops_validated_next <= 0;
        elsif fma_done = '1' then
            done_fma(to_integer(unsigned(ind_escrita_in(DATA_WIDTH-1 downto 10)))) <= "1";
            pops_validated_next <= pops_validated_curr + 1;
        end if;
    end process;
              
    -- next state logic + Moore logic
    process(curr_state, empty_fifo, valid_fifo, dirtybit_value)
    begin
        -- default values
        read_fifo <= '0';
        fma_ready <= '0';
        write_b <= (others => (others => '0'));
        pops_next <= pops_curr;
		next_state <= curr_state ;  
        
        case curr_state is
        
            -- estado de reset
            when reset =>
                the_end <= '0';
                next_state <= check_state;
                
            -- estado inicial de verificacao da fifo ou de termino da computacao 
            -- com todas as instrucoes recebidas do packager, todos os valores que entraram no FMA = # valores que sairam
            -- e fifos de entrada do processador vazias
            when check_state => 
                if empty_fifo = '0' then
                    read_fifo <= '1';
                    next_state <= state_0;
                elsif (done = 0 and pops_curr = pops_validated_curr and empty_fifo = '1') then
                    the_end <= '1';
                end if;
                
            when state_0 =>
                if valid_fifo = '1' then
                    pops_next <= pops_curr + 1;
                    next_state <= state_1;
                end if;
                
            -- verifica se houve colisão, espera pelo resultado do FMA e mais um ciclo para o valor estar à saída da mem
            when state_1 =>
                if dirtybit_value(to_integer(unsigned(ind_leitura_in(DATA_WIDTH-1 downto 10)))) = "0" then
                    next_state <= state_2;
                end if;
					 
            when state_2 =>
                if dirtybit_value(to_integer(unsigned(ind_leitura_in(DATA_WIDTH-1 downto 10)))) = "0" then
                    fma_ready <= '1';
                    write_b(to_integer(unsigned(ind_leitura_in(DATA_WIDTH-1 downto 10)))) <= "1";
                    if empty_fifo = '0' then
                        read_fifo <= '1';
                        next_state <= state_0;
                    else
                        next_state <= check_state;
                    end if;
                end if; 
					 
        end case;
    end process;
    
end Behavioral;
