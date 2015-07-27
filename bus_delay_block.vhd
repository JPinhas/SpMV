library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.MATH_REAL.ALL;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

library mylibs_v1_00_a;
use mylibs_v1_00_a.float_pkg.all;

entity bus_delay_block is
	generic(
        bits : integer
	);
	port
	(
		CLK				:	in	std_logic;
		RESET			:	in	std_logic;
		
		A				:	in	std_logic_vector(bits-1 downto 0);
		A_DELAYED		:	out std_logic_vector(bits-1 downto 0);
		COLLISION		:	out std_logic
	);
end bus_delay_block;

architecture delay_block_arch of bus_delay_block is

	type intermediate is array (8 downto 0) of std_logic_vector(bits-1 downto 0);
	signal int : intermediate;
	signal or1, or2, or3, or4 : std_logic;

begin
	process (CLK,int)
	begin
		if(CLK'event and CLK = '1') then
		
			int(0) <= A;
			
			for i in 1 to 8 loop
				int(i) <= int(i-1);
			end loop;
			
		end if;
	end process;

	A_DELAYED <= int(7);
	or1 <= '1' when A = int(0) OR A = int(1) else '0';
	or2 <= '1' when A = int(2) OR A = int(3) else '0';
	or3 <= '1' when A = int(4) OR A = int(5) else '0';
	or4 <= '1' when A = int(6) OR A = int(8) else '0';
	
	COLLISION <= or1 OR or2 OR or3 OR or4;
	
end delay_block_arch;