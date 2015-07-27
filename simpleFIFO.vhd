----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    15:55:25 06/20/2014 
-- Design Name: 
-- Module Name:    fifo_prototype - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
-- 	This fifo is written and read when the fifo is full or empty respectivelly.
-- 	The control is done internally.
--		Correct data count is not implemented.
----------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

library mylibs_v1_00_a;
use mylibs_v1_00_a.PE_Pkg.all;
use mylibs_v1_00_a.float_pkg.all;
use mylibs_v1_00_a.SoC_Pkg.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity simpleFIFO is generic
(
	DATA_WIDTH	 		: integer := 32;
	FIFO_LENGTH			: integer := 64;
	LOG2_FIFO_LENGTH	: integer := 6
);
port
(
	CLK 			: in 	std_logic;
	RST 			: in 	std_logic;
	WR_EN			: in 	std_logic;
	DIN 			: in 	std_logic_vector(DATA_WIDTH-1 downto 0);
	RD_EN			: in 	std_logic;
	DOUT 			: out std_logic_vector(DATA_WIDTH-1 downto 0);
	FIFO_EMPTY	: out std_logic;
	FIFO_VALID	: out std_logic;
	FIFO_FULL	: out std_logic;
	DATA_COUNT	: out std_logic_vector(LOG2_FIFO_LENGTH-1 downto 0)
);
end simpleFIFO;

architecture Behavioral of simpleFIFO is
	
	signal zeros, ptr_reg, ptr_next, rd_ptr_succ, wr_ptr_succ: std_logic_vector(LOG2_FIFO_LENGTH downto 0);
	signal r_en, w_en, wr_fifo, rd_fifo : std_logic;
	signal empty_reg, full_reg, empty_next, full_next : std_logic;
	signal valid_reg : std_logic;
	signal wr_op : std_logic_vector(1 downto 0);
	
	signal d_in : std_logic_vector(DATA_WIDTH-1 downto 0);
	signal d_out_reg, d_out_next, fifo_out : std_logic_vector(DATA_WIDTH-1 downto 0);
	signal fifo_rdaddr : std_logic_vector(LOG2_FIFO_LENGTH-1 downto 0);
	
	signal DIN_REG : std_logic_vector(DATA_WIDTH-1 downto 0);
	signal WR_EN_REG : std_logic;	signal RD_EN_REG : std_logic;
	
begin
	zeros <= (others => '0');
	---------------------------------------------------------------------------------------
	-- FIFO INPUTS																								 --
	---------------------------------------------------------------------------------------

	DIN_REG <= DIN;
	WR_EN_REG <= WR_EN;	
	RD_EN_REG <= RD_EN;
	
	d_in <= DIN_REG;
	r_en <= RD_EN_REG; 
	w_en <= WR_EN_REG;
	---------------------------------------------------------------------------------------
	-- FIFO 																										 --
	--------------------------------------------------------------------------------------- 
	
--	fifo_rdaddr <= zeros when empty_reg = '1' else rd_ptr_succ(LOG2_FIFO_LENGTH-1 downto 0);
	fifo_rdaddr <= rd_ptr_succ(LOG2_FIFO_LENGTH-1 downto 0);
	
	Inst_fifo_core: fifo_core 
	GENERIC MAP
	(
		DATA_WIDTH => DATA_WIDTH,
		FIFO_DEPTH => FIFO_LENGTH,
		LOG2FIFO_DEPTH	=> LOG2_FIFO_LENGTH
	)
	PORT MAP(
		CLK => CLK,
		CE => wr_fifo,
		DIN => d_in,
		SHIFT => fifo_rdaddr,
		DOUT => fifo_out
	);
	
	---------------------------------------------------------------------------------------
	-- FIFO CONTROL																							 --
	--------------------------------------------------------------------------------------- 
	
	FIFO_ADDRESS_POINTERS_REG : process(CLK, RST)
	begin
		if (CLK'event and CLK='1') then
			if (RST = '1') then
				ptr_reg <= (others => '0');
				full_reg <= '0';
				empty_reg <= '1';
				
				valid_reg <= '0';
				d_out_reg <= (others => '0');
			else
				ptr_reg <= ptr_next;
				full_reg <= full_next;
				empty_reg <= empty_next;
				
				valid_reg <= rd_fifo;
				d_out_reg <= d_out_next; 
			end if;
		end if;
	end process FIFO_ADDRESS_POINTERS_REG;
	
	-- successive value for the write and read pointers
	wr_ptr_succ <= ptr_reg + 1;
	rd_ptr_succ <= ptr_reg - 1;
	-- next state logic
	wr_fifo <= w_en and (not full_reg);
	rd_fifo <= r_en and (not empty_reg);
	wr_op <= wr_fifo & rd_fifo; --w_en & r_en;
	process(ptr_reg, wr_ptr_succ, ptr_reg, rd_ptr_succ, wr_op, empty_reg, full_reg, fifo_out) 
	begin
	-- per omission
	ptr_next <= ptr_reg;
	full_next <= full_reg;
	empty_next <= empty_reg;
	d_out_next <= d_out_reg;
	case wr_op is
		when "00" => -- no op
		when "10" => -- write
--			if (full_reg /= '1') then --not full
				ptr_next <= wr_ptr_succ;
				empty_next <= '0';
				full_next <= wr_ptr_succ(LOG2_FIFO_LENGTH);
--			end if;
		when "01" => -- write
--			if (empty_reg /= '1') then --not full
				ptr_next <= rd_ptr_succ;
				full_next <= '0';
				d_out_next <= fifo_out;
				if (rd_ptr_succ=zeros) then
					empty_next <= '1';
				end if;
--			end if;
		when others => --write/read
			d_out_next <= fifo_out;
	end case;
	end process;
	---------------------------------------------------------------------------------------
	-- FIFO OUTPUTS																							 --
	--------------------------------------------------------------------------------------- 	
	DOUT <= d_out_reg;
	FIFO_EMPTY <= empty_reg;
	FIFO_VALID <= valid_reg;
	FIFO_FULL <= full_reg;
	DATA_COUNT <= ptr_reg(LOG2_FIFO_LENGTH-1 downto 0);
		
end Behavioral;

