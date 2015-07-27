library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

library mylibs_v1_00_a;
use mylibs_v1_00_a.float_pkg.all;

entity MEMC is
generic(
	DATA_WIDTH : INTEGER := 32;
	SIZE : integer := 1024
);
port(
	clk : IN std_logic;
--	ena : in std_logic;
    wea : IN std_logic;
    addra : IN std_logic_vector(ceil_log2(SIZE)-1 DOWNTO 0);
    dina : IN std_logic_vector(DATA_WIDTH-1 DOWNTO 0);
    addrb : IN std_logic_vector(ceil_log2(SIZE)-1 DOWNTO 0);
    doutb : OUT std_logic_vector(DATA_WIDTH-1 DOWNTO 0)
);
end MEMC;

architecture Behavioral of MEMC is

	type ram_type is array (SIZE-1 downto 0) of std_logic_vector (DATA_WIDTH-1 downto 0);
	signal BRAM : ram_type := (others => (others => '0'));

begin

	process (clk)
	begin
		if (clk'event and clk = '1') then
            if (wea = '1') then
                BRAM(conv_integer(addra)) <= dina;
            end if;
            doutb <= BRAM(conv_integer(addrb));
		end if;
	end process;

end Behavioral;