----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    15:54:45 05/22/2014 
-- Design Name: 
-- Module Name:    right_shifter - Behavioral 
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
--
----------------------------------------------------------------------------------library ieee;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

library mylibs_v1_00_a;
use mylibs_v1_00_a.PE_Pkg.all;
use mylibs_v1_00_a.float_pkg.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity right_shifter is generic
(
	DIN_WIDTH : integer := 53;
	DATA_WIDTH : integer := 173;
	DOUT_WIDTH : integer := 120;
	MAX_SHIFT : integer := 120;
	SHIFT_WIDTH : integer := 8;
	DISCARDED_WIDTH : integer := 53
);
Port ( 
	clk 					: in std_logic;
	rst 					: in std_logic;
	ce	 					: in std_logic;
	din 					: in std_logic_vector(DIN_WIDTH-1 downto 0);  -- 256-bits in
	shift 				: in std_logic_vector(SHIFT_WIDTH-1 downto 0); --(5 downto 0);  -- 6-bits shift-control
	sign 					: in std_logic;
	signreg				: in std_logic;
--	sticky_bit: out std_logic;
	discarded_bits		: out std_logic_vector(DISCARDED_WIDTH-1 downto 0);	
	dout 					: out std_logic_vector(DOUT_WIDTH downto 0)
 );
end right_shifter;

architecture Behavioral of right_shifter is

	signal d0, d1, d2, d3, d4_temp, zero : std_logic_vector(DATA_WIDTH-1 downto 0);
	signal d4, d4reg : std_logic_vector(DOUT_WIDTH-1 downto 0); -- does not have sign yet
	signal d0reg, d1reg, d2reg, d3reg : std_logic_vector(d0'range);
	signal sh0Reg, sh1Reg : std_logic_vector(shift'range);
	signal discarded_bitsReg : std_logic_vector(DISCARDED_WIDTH-1 downto 0);	
--	signal sticky_bit_tmp, stickybitReg : std_logic;
	signal sign0reg, sign1reg, sign2reg, sign3reg, sign4reg : std_logic;

begin

	zero <= (others=>'0');
	
	d0 <= din & zero(DATA_WIDTH-DIN_WIDTH-1 downto 0);
		 
	--  desloca 0,1,2,3 bits
	d1 <= zero(d1'high downto d1'high-2) & d0reg(d1'high downto 3) when sh0Reg(1 downto 0) = "11" else
			zero(d1'high downto d1'high-1) & d0reg(d1'high downto 2) when sh0Reg(1 downto 0) = "10" else
			zero(d1'high) & d0reg(d1'high downto 1) when sh0Reg(1 downto 0) = "01" else
			d0reg;

	--  desloca 0,4,8,12 bits
	d2 <= zero(d1'high downto d1'high-11) & d1reg(d1'high downto 12) when sh0Reg(3 downto 2) = "11" else
	   	zero(d1'high downto d1'high-7)  & d1reg(d1'high downto 8) when sh0Reg(3 downto 2) = "10" else
			zero(d1'high downto d1'high-3)  & d1reg(d1'high downto 4) when sh0Reg(3 downto 2) = "01" else
			d1reg;

	--  desloca 0,16,32,48 bits
	d3 <= zero(d1'high downto d1'high-47) & d2reg(d1'high downto 48) when sh0Reg(5 downto 4) = "11" else
		   zero(d1'high downto d1'high-31) & d2reg(d1'high downto 32) when sh0Reg(5 downto 4) = "10" else
		   zero(d1'high downto d1'high-15) & d2reg(d1'high downto 16) when sh0Reg(5 downto 4) = "01" else
		   d2reg;

	SMALLER_SHIFT	:	if( MAX_SHIFT < 64 ) generate 
		
		--  desloca 0, 64, 128, 192 bits
		d4_temp <= d3reg;			
		
	end generate; -- SMALLER_SHIFT
	
	BIGGER_SHIFT	:	if( MAX_SHIFT >= 64 ) generate 
		
		--  desloca 0, 64, 128, 192 bits
		d4_temp <= 	zero(d1'high downto d1'high-63) & d3reg(d1'high downto 64) 	when sh1Reg(6) = '1' else
						d3reg;			
		
	end generate; -- BIGGER_SHIFT
	
	---------------------------------------------------------------------------------------------------------
	-- Negate shift result in case different operands signs exist -------------------------------------------
	
	d4 <= not( d4_temp(DATA_WIDTH-1 downto DISCARDED_WIDTH) ) when sign = '1' else d4_temp(DATA_WIDTH-1 downto DISCARDED_WIDTH);
	
	-- REGISTERS --------------------------------------------------------------------------------------------
	process(clk)
	begin
		if (rising_edge(clk)) then
			if (rst = '1') then
				d0reg <= (others => '0');
	--			d1reg <= (others => '0');
	--			d2reg <= (others => '0');
				d3reg <= (others => '0');
	--			d4reg <= (others => '0');
	--			discarded_bitsReg <= (others => '0');
	--			stickybitReg <= '0'; --sticky_bit_tmp;
	--			sh0Reg <= (others => '0');
				sh1Reg <= (others => '0');
			elsif (CE = '1') then
				d0reg <= d0;
	--			d1reg <= d1;
	--			d2reg <= d2;
				d3reg <= d3;
	--			d4reg <= d4;
	--			discarded_bitsReg <= d4_temp(DISCARDED_WIDTH-1 downto 0);
	--			stickybitReg <= '0'; --sticky_bit_tmp;
	--			sh0Reg <= shift;
				sh1Reg <= sh0Reg;
			end if;
		end if;
	end process;

--	d0reg <= d0;
	d1reg <= d1;
	d2reg <= d2;
--	d3reg <= d3;
	d4reg <= d4;
	discarded_bitsReg <= d4_temp(DISCARDED_WIDTH-1 downto 0);
--	stickybitReg <= sticky_bit_tmp
	sh0Reg <= shift;
--	sh1Reg <= sh0Reg;

-- OUTPUT ------------------------------------------------------------------------------------------------
dout <= signreg & d4reg; 
--sticky_bit <= stickybitReg;
discarded_bits <= discarded_bitsReg;
	
end Behavioral;

