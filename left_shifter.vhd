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
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity left_shifter is generic
(
	DIN_WIDTH : integer := 160;
	DOUT_WIDTH : integer := 54;
	MAX_SHIFT : integer := 160;
	SHIFT_WIDTH : integer := 8
);
Port ( 
  clk : in std_logic;
  rst : in std_logic;
  ce  : in std_logic;
  din : in std_logic_vector(DIN_WIDTH-1 downto 0);  -- 256-bits in
  shift : in std_logic_vector(SHIFT_WIDTH-1 downto 0); --(5 downto 0);  -- 6-bits shift-control
  sticky_bit : out std_logic;
  dout : out std_logic_vector(DOUT_WIDTH-1 downto 0)
 );
end left_shifter;

architecture Behavioral of left_shifter is

	signal d0, d1, d2, d3, d4, zero : std_logic_vector(DIN_WIDTH-1 downto 0);
	signal d0reg, d1reg, d2reg, d3reg, d4reg : std_logic_vector(d0'range);
	signal shReg : std_logic_vector(shift'range); 

begin

	zero <= (others=>'0');
	
--	d0 <= zero(static_max_shift-DIN_WIDTH-1 downto 0) & din & zero(MAX_SHIFT-1 downto 0); -- zero(d0'high downto din'high+1) & 
	d0 <= din;
		 
	--  desloca 0,1,2,3 bits
	d1 <= d0reg(d1'high-3 downto 0) & zero(d1'high downto d1'high-2) when shReg(1 downto 0) = "11" else
			d0reg(d1'high-2 downto 0) & zero(d1'high downto d1'high-1) when shReg(1 downto 0) = "10" else
			d0reg(d1'high-1 downto 0) & zero(d1'high) when shReg(1 downto 0) = "01" else
			d0reg;

	--  desloca 0,4,8,12 bits
	d2 <= d1reg(d1'high-12 downto 0) & zero(d1'high downto d1'high-11) when shReg(3 downto 2) = "11" else
	   	d1reg(d1'high-8 downto 0) & zero(d1'high downto d1'high-7) when shReg(3 downto 2) = "10" else
			d1reg(d1'high-4 downto 0) & zero(d1'high downto d1'high-3) when shReg(3 downto 2) = "01" else
			d1reg;

	--  desloca 0,16,32,48 bits
	d3 <= d2reg(d1'high-48 downto 0) & zero(d1'high downto d1'high-47) when shReg(5 downto 4) = "11" else
		   d2reg(d1'high-32 downto 0) & zero(d1'high downto d1'high-31) when shReg(5 downto 4) = "10" else
		   d2reg(d1'high-16 downto 0) & zero(d1'high downto d1'high-15) when shReg(5 downto 4) = "01" else
		   d2reg;

	SMALLER_SHIFT	:	if( MAX_SHIFT < 64 ) generate 
		
		--  desloca 0, 64, 128, 192 bits
		d4 <= d3reg;
	
	end generate; -- SMALLER_SHIFT
	
	BIGGER_SHIFT	:	if( MAX_SHIFT >= 64 ) generate 
		
		--  desloca 0, 64, 128, 192 bits
		d4 <= d3reg(d1'high-64 downto 0) & zero(d1'high downto d1'high-63) when shReg(6) = '1' else
				d3reg;
		
	end generate; -- BIGGER_SHIFT
	
	dout <= d4reg(DIN_WIDTH-1 downto DIN_WIDTH-DOUT_WIDTH); --(MAX_SHIFT+DIN_WIDTH-1 downto MAX_SHIFT); -- & sticky_bit;
	  
	process(clk)
	begin
		if (rising_edge(clk)) then
			if (RST = '1') then
				d0reg <= (others => '0');
	--			d1reg <= (others => '0');
	--			d2reg <= (others => '0');
	--			d3reg <= (others => '0');
				d4reg <= (others => '0');
	--			shReg <= (others => '0');
			elsif (CE = '1') then 
				d0reg <= d0;
	--			d1reg <= d1;
	--			d2reg <= d2;
	--			d3reg <= d3;
				d4reg <= d4;
	--			shReg <= shift;
			end if;
		end if;
	end process;

--	d0reg <= d0;
	d1reg <= d1;
	d2reg <= d2;
	d3reg <= d3;
--	d4reg <= d4;
	shReg <= shift; -- comes delayed already from the outside
	
end Behavioral;

