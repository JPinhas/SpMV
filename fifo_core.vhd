----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    16:54:15 06/24/2014 
-- Design Name: 
-- Module Name:    lixo - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: design is based on a minimum size of 32 for fifo depth.
--
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
library UNISIM;
use UNISIM.VComponents.all;

entity fifo_core is
generic
(
	DATA_WIDTH		: integer := 32;
	FIFO_DEPTH 		: integer := 64;
	LOG2FIFO_DEPTH	: integer := 6
);
port
(
	CLK : in std_logic;
	CE : in std_logic;
	DIN : in std_logic_vector(DATA_WIDTH-1 downto 0);
	SHIFT : in std_logic_vector(LOG2FIFO_DEPTH-1 downto 0);
	DOUT : out std_logic_vector(DATA_WIDTH-1 downto 0)
);
end fifo_core;

architecture Behavioral of fifo_core is

	constant fixed_dwidth : integer := 32; -- constant fixed data width
	constant fixed_swidth : integer := 5; -- constant fixed shift width
	constant nr_shifters : integer := FIFO_DEPTH/fixed_dwidth;
	
	type data_type is array(nr_shifters-1 downto 0) of std_logic_vector(DATA_WIDTH-1 downto 0);
	signal dout_temp, dout_cas : data_type;
	
begin

--	SHIFTERS :  --label name
--		for i in 0 to DATA_WIDTH-1 generate   --D DSP is instantiated N_DSPS times.
--		begin  --"begin" statement for "generate"
--			SRLC32E_inst : SRLC32E
--			generic map (
--				INIT => X"00000000")
--			port map (
--				Q => DOUT(i),     -- SRL data output
--				Q31 => DOUT_CAS(i),    -- SRL cascade output pin
--				A => SHIFT,    -- 5-bit shift depth select input
--				CE => CE,      -- Clock enable input
--				CLK => CLK,    -- Clock input
--				D => DIN(i)         -- SRL data input
--			);
--	
--		end generate SHIFTERS;  --end "generate" block.

	SHIFTER_0 :  --label name		
		for i in 0 to DATA_WIDTH-1 generate   
		begin  --"begin" statement for "generate"
			SRLC32E_inst : SRLC32E
			generic map (
				INIT => X"00000000")
			port map (
				Q => dout_temp(0)(i),     -- SRL data output
				Q31 => dout_cas(0)(i),    -- SRL cascade output pin
				A => SHIFT(fixed_swidth-1 downto 0),    -- 5-bit shift depth select input
				CE => CE,      -- Clock enable input
				CLK => CLK,    -- Clock input
				D => DIN(i)         -- SRL data input
			);
		end generate SHIFTER_0;
				
	BIGGER_FIFO:	if( nr_shifters > 1 ) generate 
	
		SHIFTERS :  --label name		
		for k in 1 to nr_shifters-1 generate   --D DSP is instantiated N_DSPS times.
		begin
			SHIFTER_32BITS :  --label name		
				for i in 0 to DATA_WIDTH-1 generate   
				begin  --"begin" statement for "generate"
					
					SRLC32E_inst : SRLC32E
					generic map (
						INIT => X"00000000")
					port map (
						Q => dout_temp(k)(i), --DOUT(i),     -- SRL data output
						Q31 => dout_cas(k)(i),    -- SRL cascade output pin
						A => SHIFT(fixed_swidth-1 downto 0),    -- 5-bit shift depth select input
						CE => CE,      -- Clock enable input
						CLK => CLK,    -- Clock input
						D => dout_cas(k-1)(i) --DIN(i)         -- SRL data input
					);				
			end generate SHIFTER_32BITS;
		end generate SHIFTERS;  --end "generate" block.
		
	end generate; 	 --BIGGER_FIFO

	DOUT <= dout_temp(conv_integer( SHIFT(LOG2FIFO_DEPTH-1 downto fixed_swidth) ));
	
end Behavioral;

