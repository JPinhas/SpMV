----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    21:53:17 05/09/2014 
-- Design Name: 
-- Module Name:    dsp_wrapper_0 - Behavioral 
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
library UNISIM;
use UNISIM.VComponents.all;

entity dsp_wrapper_0 is generic
(
	DSP_A_WIDTH : integer := 30;
	DSP_B_WIDTH : integer := 18;
	DSP_C_WIDTH : integer := 48;
	DSP_P_WIDTH : integer := 48;
	AREG_NR		: integer := 1; 
	BREG_NR		: integer := 1;
	CREG_NR		: integer := 1
);
port
(
	CLK			: in 	std_logic;
	RST			: in 	std_logic;
	CE 			: in 	std_logic;
	dsp_ACIN		: in 	std_logic_vector(DSP_A_WIDTH-1 downto 0);
	dsp_A 		: in 	std_logic_vector(DSP_A_WIDTH-1 downto 0);
	dsp_BCIN		: in 	std_logic_vector(DSP_B_WIDTH-1 downto 0);
	dsp_B			: in 	std_logic_vector(DSP_B_WIDTH-1 downto 0);
	dsp_C			: in 	std_logic_vector(DSP_C_WIDTH-1 downto 0);
	dsp_PCIN		: in 	std_logic_vector(DSP_P_WIDTH-1 downto 0);
	dsp_CARRYIN	: in 	std_logic;
	inmode_in	: in	std_logic_vector(4 downto 0); 
	opmode_in	: in	std_logic_vector(2 downto 0); 
	dsp_ACOUT	: out	std_logic_vector(DSP_A_WIDTH-1 downto 0);
	dsp_BCOUT	: out	std_logic_vector(DSP_B_WIDTH-1 downto 0);
	dsp_PCOUT	: out	std_logic_vector(DSP_P_WIDTH-1 downto 0);
	dsp_P 		: out	std_logic_vector(DSP_P_WIDTH-1 downto 0)
);
end dsp_wrapper_0;

architecture Behavioral of dsp_wrapper_0 is

	constant dsp_d_width : integer := 25;	
	constant inmode_width : integer := 5;
	constant alumode_width : integer := 4;
	constant opcode_width : integer := 7;
	constant carryinsel_width : integer := 3;
	
	signal zeros : std_logic_vector(DSP_P_WIDTH-1 downto 0);
	
	signal dsp_D : std_logic_vector(dsp_d_width-1 downto 0);
	signal INMODE: std_logic_vector(inmode_width-1 downto 0);
	signal ALUMODE : std_logic_vector(alumode_width-1 downto 0);
	signal OPMODE : std_logic_vector(opcode_width-1 downto 0);
	signal CARRYINSEL : std_logic_vector(carryinsel_width-1 downto 0);
	
begin

	zeros <= (others => '0');
	
	-- DSP0 inputs
	-- Data
	dsp_D <= zeros(24 downto 0);
	
	-- Control signals
	ALUMODE <= "0000";
	CARRYINSEL <= "000";
	INMODE <= "00000"; -- Multiplier A port - A2 ; Multiplier B port - B2
--	OPMODE <= opmode_in(1) & "0" & opmode_in(0) & "0101"; -- mode decide if we do Z multiplexer output is zero, PCIN or 17-bit shifted PCIN
	OPMODE <= opmode_in & "0101"; -- mode decide if we do Z multiplexer output is zero, PCIN or 17-bit shifted PCIN
	
	-- DSP48E1: 48-bit Multi-Functional Arithmetic Block
   --          Virtex-7
   -- Xilinx HDL Language Template, version 14.5

   DSP48E1_inst : DSP48E1
   generic map (
      -- Feature Control Attributes: Data Path Selection
      A_INPUT => "DIRECT",               -- Selects A input source, "DIRECT" (A port) or "CASCADE" (ACIN port)
      B_INPUT => "DIRECT",               -- Selects B input source, "DIRECT" (B port) or "CASCADE" (BCIN port)
      USE_DPORT => FALSE,                -- Select D port usage (TRUE or FALSE)
      USE_MULT => "MULTIPLY",            -- Select multiplier usage ("MULTIPLY", "DYNAMIC", or "NONE")
      -- Pattern Detector Attributes: Pattern Detection Configuration
      AUTORESET_PATDET => "NO_RESET",    -- "NO_RESET", "RESET_MATCH", "RESET_NOT_MATCH" 
      MASK => X"FFFFFFFFFFFF", -- X"0000All ones",           -- 48-bit mask value for pattern detect (1=ignore)
      PATTERN => X"000000000000", --X"000All zeros",        -- 48-bit pattern match for pattern detect
      SEL_MASK => "MASK",                -- "C", "MASK", "ROUNDING_MODE1", "ROUNDING_MODE2" 
      SEL_PATTERN => "PATTERN",          -- Select pattern value ("PATTERN" or "C")
      USE_PATTERN_DETECT => "NO_PATDET", -- Enable pattern detect ("PATDET" or "NO_PATDET")
      -- Register Control Attributes: Pipeline Register Configuration
      ACASCREG => AREG_NR,               -- Number of pipeline stages between A/ACIN and ACOUT (0, 1 or 2)
      ADREG => 1,                        -- Number of pipeline stages for pre-adder (0 or 1)
      ALUMODEREG => 1,                   -- Number of pipeline stages for ALUMODE (0 or 1)
      AREG => AREG_NR,                   -- Number of pipeline stages for A (0, 1 or 2)
      BCASCREG => BREG_NR,               -- Number of pipeline stages between B/BCIN and BCOUT (0, 1 or 2)
      BREG => BREG_NR,                   -- Number of pipeline stages for B (0, 1 or 2)
      CARRYINREG => 1,                   -- Number of pipeline stages for CARRYIN (0 or 1)
      CARRYINSELREG => 1,                -- Number of pipeline stages for CARRYINSEL (0 or 1)
      CREG => CREG_NR,                         -- Number of pipeline stages for C (0 or 1)
      DREG => 1,                         -- Number of pipeline stages for D (0 or 1)
      INMODEREG => 1,                    -- Number of pipeline stages for INMODE (0 or 1)
      MREG => 1,                         -- Number of multiplier pipeline stages (0 or 1)
      OPMODEREG => 1,                    -- Number of pipeline stages for OPMODE (0 or 1)
      PREG => 1,                         -- Number of pipeline stages for P (0 or 1)
      USE_SIMD => "ONE48"                -- SIMD selection ("ONE48", "TWO24", "FOUR12")
   )
   port map (
      -- Cascade: 30-bit (each) output: Cascade Ports
      ACOUT => dsp_ACOUT,                   -- 30-bit output: A port cascade output
      BCOUT => dsp_BCOUT,                   -- 18-bit output: B port cascade output
      CARRYCASCOUT => open,     -- 1-bit output: Cascade carry output
      MULTSIGNOUT => open,       -- 1-bit output: Multiplier sign cascade output
      PCOUT => dsp_PCOUT,                   -- 48-bit output: Cascade output
      -- Control: 1-bit (each) output: Control Inputs/Status Bits
      OVERFLOW => open,             -- 1-bit output: Overflow in add/acc output
      PATTERNBDETECT => open, -- 1-bit output: Pattern bar detect output
      PATTERNDETECT => open,   -- 1-bit output: Pattern detect output
      UNDERFLOW => open,           -- 1-bit output: Underflow in add/acc output
      -- Data: 4-bit (each) output: Data Ports
      CARRYOUT => open,             -- 4-bit output: Carry output
      P => dsp_P,                           -- 48-bit output: Primary data output
      -- Cascade: 30-bit (each) input: Cascade Ports
      ACIN => dsp_ACIN,                     -- 30-bit input: A cascade data input
      BCIN => dsp_BCIN,                     -- 18-bit input: B cascade input
      CARRYCASCIN => '0',       -- 1-bit input: Cascade carry input
      MULTSIGNIN => '0',         -- 1-bit input: Multiplier sign input
      PCIN => dsp_PCIN,                     -- 48-bit input: P cascade input
      -- Control: 4-bit (each) input: Control Inputs/Status Bits
      ALUMODE => ALUMODE,               -- 4-bit input: ALU control input
      CARRYINSEL => CARRYINSEL,         -- 3-bit input: Carry select input
      CEINMODE => CE,             -- 1-bit input: Clock enable input for INMODEREG
      CLK => CLK,                       -- 1-bit input: Clock input
      INMODE => INMODE,                 -- 5-bit input: INMODE control input
      OPMODE => OPMODE,                 -- 7-bit input: Operation mode input
      RSTINMODE => RST,           -- 1-bit input: Reset input for INMODEREG
      -- Data: 30-bit (each) input: Data Ports
      A => dsp_A,                           -- 30-bit input: A data input
      B => dsp_B,                           -- 18-bit input: B data input
      C => dsp_C,                           -- 48-bit input: C data input
      CARRYIN => dsp_CARRYIN,               -- 1-bit input: Carry input signal
      D => dsp_D,                           -- 25-bit input: D data input
      -- Reset/Clock Enable: 1-bit (each) input: Reset/Clock Enable Inputs
      CEA1 => CE,                     -- 1-bit input: Clock enable input for 1st stage AREG
      CEA2 => CE,                     -- 1-bit input: Clock enable input for 2nd stage AREG
      CEAD => CE,                     -- 1-bit input: Clock enable input for ADREG
      CEALUMODE => CE,           -- 1-bit input: Clock enable input for ALUMODERE
      CEB1 => CE,                     -- 1-bit input: Clock enable input for 1st stage BREG
      CEB2 => CE,                     -- 1-bit input: Clock enable input for 2nd stage BREG
      CEC => CE,                       -- 1-bit input: Clock enable input for CREG
      CECARRYIN => CE,           -- 1-bit input: Clock enable input for CARRYINREG
      CECTRL => CE,                 -- 1-bit input: Clock enable input for OPMODEREG and CARRYINSELREG
      CED => CE,                       -- 1-bit input: Clock enable input for DREG
      CEM => CE,                       -- 1-bit input: Clock enable input for MREG
      CEP => CE,                       -- 1-bit input: Clock enable input for PREG
      RSTA => RST,                     -- 1-bit input: Reset input for AREG
      RSTALLCARRYIN => RST,   -- 1-bit input: Reset input for CARRYINREG
      RSTALUMODE => RST,         -- 1-bit input: Reset input for ALUMODEREG
      RSTB => RST,                     -- 1-bit input: Reset input for BREG
      RSTC => RST,                     -- 1-bit input: Reset input for CREG
      RSTCTRL => RST,               -- 1-bit input: Reset input for OPMODEREG and CARRYINSELREG
      RSTD => RST,                     -- 1-bit input: Reset input for DREG and ADREG
      RSTM => RST,                     -- 1-bit input: Reset input for MREG
      RSTP => RST                      -- 1-bit input: Reset input for PREG
   );

   -- End of DSP48E1_inst instantiation
	
end Behavioral;

