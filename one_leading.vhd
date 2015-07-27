----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    16:24:48 08/08/2013 
-- Design Name: 
-- Module Name:    one_leading - Behavioral 
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
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

-- This component first prepare the input signal to be processed by an encoder.
-- The encoder then gives the number of leading zeros.

entity one_leading is
generic
	(
		man_bits				: integer := 50;
		shift_bits			: integer := 6
	);
	port
	(
		--inputs
		MAN_IN				: in	std_logic_vector(man_bits-1 downto 0);
		--outputs
		SHIFT					: out	std_logic_vector(shift_bits-1 downto 0) := (others=>'0')
--		EXCEPTION_OUT		: out	std_logic := '0'
	);
end one_leading;

architecture Behavioral of one_leading is

	component onehot_to_bin is
	generic
	(
		ONEHOT_WIDTH						: integer;
		BIN_WIDTH							: integer
	);
    port(
      onehot : in std_logic_vector(ONEHOT_WIDTH-1 downto 0);          
      bin : out std_logic_vector(BIN_WIDTH-1 downto 0)
      );
  end component;
  
  signal acomp, aa, arev, bb : std_logic_vector (MAN_IN'range); 
  signal ones, shift_value : std_logic_vector(SHIFT'range);
--  signal exc : std_logic;
  
begin

--  ones <= (others => '1');
--
--  l_reverse: for i in MAN_IN'range generate
--    arev(MAN_IN'high-i) <= MAN_IN(i);
----    bb(MAN_IN'high-i) <= aa(i);
--  end generate l_reverse;
--  
--  -- eliminate ones past the the first one
--  acomp <= (not arev) + 1;
--  aa <= arev and acomp;

	aa <= MAN_IN;

	-- determine number of leading zeros 
  inst_onehot_to_bin: onehot_to_bin 
  generic map
  (
		ONEHOT_WIDTH	=>	man_bits,
		BIN_WIDTH		=>	shift_bits
	)
  port map(
    onehot => aa,
    bin => shift_value
    );
	 
--	exc <= '1' when shift_value > ones else '0';
	
	SHIFT <= shift_value;
--	EXCEPTION_OUT <= exc;
	
end Behavioral;

