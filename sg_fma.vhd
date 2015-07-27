----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    12:50:17 05/09/2014 
-- Design Name: 
-- Module Name:    sg_man_mul - Behavioral 
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

entity sg_fma is generic
(
	DISCARDED_ADDEND_WIDTH : integer := 53;
	MUL_RES_WIDTH : integer := 106;
	DEF_MUL_WIDTH : integer := 106;
	RIGHTMOST_WIDTH : integer := 34;
	LEFTMOST_WIDTH : integer := 55;
	ADDITION_WIDTH : integer := 161;
	COL_WIDTH : integer := 17;
	NB_COL	 : integer := 5;
	EXP_WIDTH : integer := 11;
	RES_WIDTH : integer := 64
);
port
(
	CLK					: in 	std_logic;
	RST					: in 	std_logic;
	CE 					: in 	std_logic;
	ready					: in 	std_logic;
	dsp_ina 				: in	dsp_ina_type;
	dsp_inb 				: in	dsp_inb_type;
	dsp_inc 				: in	dsp_inc_type;
	-- END: Modified if number of dsps changes ----------------------------
	leftmost_bits 		: in  std_logic_vector(LEFTMOST_WIDTH-1 downto 0);  
	discarded_bits		: in  std_logic_vector(DISCARDED_ADDEND_WIDTH-1 downto 0);
	xy_sign 				: in 	std_logic;
	w_sign 				: in 	std_logic;
	diff_sign 			: in 	std_logic;
	exp 					: in 	std_logic_vector(EXP_WIDTH-1 downto 0);
	-- outputs
	rounded_res			: out	std_logic_vector(RES_WIDTH-1 downto 0);
	done 					: out	std_logic
);
end sg_fma;

architecture Behavioral of sg_fma is
	
	COMPONENT dsp_wrapper_0
	GENERIC
	(
		DSP_A_WIDTH : integer := 30;
		DSP_B_WIDTH : integer := 18;
		DSP_C_WIDTH : integer := 48;
		DSP_P_WIDTH : integer := 48;
		AREG_NR		: integer := 1; 
		BREG_NR		: integer := 1;
		CREG_NR		: integer := 1
	);
	PORT(
		CLK				: in 	std_logic;
		RST				: in 	std_logic;
		CE 				: in 	std_logic;
		dsp_ACIN			: in 	std_logic_vector(DSP_A_WIDTH-1 downto 0);
		dsp_A 			: in 	std_logic_vector(DSP_A_WIDTH-1 downto 0);
		dsp_BCIN			: in 	std_logic_vector(DSP_B_WIDTH-1 downto 0);
		dsp_B				: in 	std_logic_vector(DSP_B_WIDTH-1 downto 0);
		dsp_C				: in 	std_logic_vector(DSP_C_WIDTH-1 downto 0);
		dsp_PCIN			: in 	std_logic_vector(DSP_P_WIDTH-1 downto 0);
		dsp_CARRYIN		: in 	std_logic;
		inmode_in		: in	std_logic_vector(4 downto 0); 
		opmode_in		: in	std_logic_vector(2 downto 0); 
		dsp_ACOUT		: out	std_logic_vector(DSP_A_WIDTH-1 downto 0);
		dsp_BCOUT		: out	std_logic_vector(DSP_B_WIDTH-1 downto 0);
		dsp_PCOUT		: out	std_logic_vector(DSP_P_WIDTH-1 downto 0);
		dsp_P 			: out	std_logic_vector(DSP_P_WIDTH-1 downto 0)
	);
	END COMPONENT;
	
	-- Constants declaration
	constant exp_bits					: integer := EXP_WIDTH;
	constant shift_width				: integer := 7;
	constant dsp_p_width 			: integer := 48;
	constant discarded_mul_bits	: integer := DEF_MUL_WIDTH-MUL_RES_WIDTH;
	constant full_add_width			: integer := ADDITION_WIDTH+discarded_mul_bits;
	constant add_res_width 			: integer := ADDITION_WIDTH;
	constant add_temp_width 		: integer := full_add_width-RIGHTMOST_WIDTH;
	constant norm_res_width 		: integer := add_res_width-1;
--	constant discarded_bits_width : integer := add_res_width-norm_res_width;
	constant unrounded_res_bits	: integer := norm_res_width+exp_bits;
	constant round_man_bits 		: integer := RES_WIDTH-exp_bits-1;
	
	-- Signals
	
	-- BEGIN: Modified if number of dsps changes ----------------------------------------------------------
	signal temp_reg0 : std_logic_vector(RIGHTMOST_WIDTH-1 downto 0);
	
	-- END: Modified if number of dsps changes ------------------------------------------------------------
	
	signal dsp_P, dsp_PCIN, dsp_PCOUT : dsp_out_type;
	
	signal sign, add_sign, sign_reg4, sign_reg5, sign_reg6, sign_reg7 : std_logic;
	signal exp_reg2, exp_reg3, exp_reg4, exp_reg5, exp_reg6, exp_reg7, exp_after_norm : std_logic_vector(EXP_WIDTH-1 downto 0);
	signal xy_sign_reg2, w_sign_reg2, diff_sign_reg2 : std_logic;
	signal xy_sign_reg3, w_sign_reg3, diff_sign_reg3 : std_logic;
	
	signal zeros : std_logic_vector(DEF_MUL_WIDTH-1 downto 0);

	signal add_sit1_op0 : std_logic_vector(LEFTMOST_WIDTH-1 downto 0);
	-- addition of the overlapped part of the result
	signal add_res : std_logic_vector(add_res_width-1 downto 0); 
	signal correct_add_res : std_logic_vector(norm_res_width-1 downto 0); 
	signal arev, acomp, aa, aa_reg5 : std_logic_vector(norm_res_width-1 downto 0);
	
	signal un_normalized_man_reg4, un_normalized_man_reg5 : std_logic_vector(norm_res_width-1 downto 0);
	signal norm_shift, norm_shift_reg6 : std_logic_vector(shift_width-1 downto 0);
	signal normalized_man : std_logic_vector(norm_res_width-1 downto 0);
	
	signal leftmost_bits_reg2, leftmost_bits_reg3 : std_logic_vector(LEFTMOST_WIDTH-1 downto 0);
	signal sticky_bit, sticky_bit_reg2, sticky_bit_reg3, sticky_bit_reg4, sticky_bit_reg5, sticky_bit_reg6, sticky_bit_reg7 : std_logic;
	
	signal unrounded_res : std_logic_vector(unrounded_res_bits-1 downto 0);
	signal rnd_res_sign : std_logic;
	signal rnd_res_exp  : std_logic_vector(exp_bits-1 downto 0);
	signal rnd_res_man  : std_logic_vector(round_man_bits-1 downto 0);
	
	signal dsp0_CARRRYIN : std_logic;
	signal done_reg2, done_reg3, done_reg4, done_reg5, done_reg6, done_reg7, done_reg8, done_reg9 : std_logic;
	
begin
	
	zeros <= (others => '0');
	
	PIPELINE_REGS: process (CLK) is
	begin
		if(rising_edge(CLK)) then
			if (RST = '1') then
				-- Third Stage
				xy_sign_reg2 <= '0';
				w_sign_reg2 <= '0';
				diff_sign_reg2 <= '0';
				
				sticky_bit_reg2 <= '0';
				
				leftmost_bits_reg2 <= (others => '0');
				
				done_reg2 <= '0';
				
				-- Forth Stage
				xy_sign_reg3 <= '0';
				w_sign_reg3 <= '0';
				diff_sign_reg3 <= '0';
				
				sticky_bit_reg3 <= '0';
				
				exp_reg3 <= (others => '0');
				
				leftmost_bits_reg3 <= (others => '0');
				
				done_reg3 <= '0';
				
				-- Fifth Stage
				sign_reg4 <= '0';
				
				sticky_bit_reg4 <= '0';
				
				exp_reg4 <= (others => '0');
				done_reg4 <= '0';
				-- Sixth Stage
	--			exp_reg5 <= (others => '0');
	--			sign_reg5 <= '0'
	--			sticky_bit_reg5 <= '0';
	--			done_reg5 <= '0';
				-- Seventh Stage
				norm_shift_reg6 <= (others => '0');
				exp_reg6 <= (others => '0');
				sign_reg6 <= '0';
				sticky_bit_reg6 <= '0';
				done_reg6 <= '0';
				-- Eighth Stage
				exp_reg7 <= (others => '0');
				sign_reg7 <= '0';
				sticky_bit_reg7 <= '0';
				done_reg7 <= '0';
				-- Nineth Stage
--				done_reg8 <= '0';
				-- Tenth Stage
				done_reg9 <= '0';
			elsif (CE = '1') then
				-- Third Stage
				xy_sign_reg2 <= xy_sign;
				w_sign_reg2 <= w_sign;
				diff_sign_reg2 <= diff_sign;
				
				sticky_bit_reg2 <= sticky_bit;
				
				leftmost_bits_reg2 <= leftmost_bits;
				
				done_reg2 <= ready;
				
				-- Forth Stage
				xy_sign_reg3 <= xy_sign_reg2;
				w_sign_reg3 <= w_sign_reg2;
				diff_sign_reg3 <= diff_sign_reg2;
				
				sticky_bit_reg3 <= sticky_bit_reg2;
				
				exp_reg3 <= exp;
				
				leftmost_bits_reg3 <= leftmost_bits_reg2;
				
				done_reg3 <= done_reg2;
				
				-- Fifth Stage
				sign_reg4 <= sign;
				
				sticky_bit_reg4 <= sticky_bit_reg3;
				
				exp_reg4 <= exp_reg3;
				done_reg4 <= done_reg3;
				-- Sixth Stage
	--			exp_reg5 <= exp_reg4;
	--			sign_reg5 <= sign_reg4;
	--			sticky_bit_reg5 <= sticky_bit_reg4;
	--			done_reg5 <= done_reg4;
				-- Seventh Stage
				norm_shift_reg6 <= norm_shift;
				exp_reg6 <= exp_reg5;
				sign_reg6 <= sign_reg5;
				sticky_bit_reg6 <= sticky_bit_reg5;
				done_reg6 <= done_reg5;
				-- Eighth Stage
				exp_reg7 <= exp_after_norm;
				sign_reg7 <= sign_reg6;
				sticky_bit_reg7 <= sticky_bit_reg6;
				done_reg7 <= done_reg6;
				-- Nineth Stage
--				done_reg8 <= done_reg7;
				-- Tenth Stage
				done_reg9 <= done_reg8;
			end if;
		end if;
	end process PIPELINE_REGS;

	exp_reg5 <= exp_reg4;
	sign_reg5 <= sign_reg4;
	sticky_bit_reg5 <= sticky_bit_reg4;
	done_reg5 <= done_reg4;
	
	done_reg8 <= done_reg7;
------------------------------------------------------------------------------------------------
-- Begin DSPs instantiation --------------------------------------------------------------------
------------------------------------------------------------------------------------------------

	-- First DSP -----------------------------------------------------------------
	DSP_0: dsp_wrapper_0 
	GENERIC MAP 
	(
		AREG_NR => 1,
		BREG_NR => 1,
		CREG_NR => 0
	)
	PORT MAP(
		CLK => CLK,
		RST => RST,
		CE => CE,
		dsp_A => dsp_ina(0),
		dsp_ACIN => zeros(29 downto 0),
		dsp_B => dsp_inb(0),
		dsp_BCIN => zeros(17 downto 0),
		dsp_C => dsp_inc(0),
		dsp_PCIN => zeros(dsp_p_width-1 downto 0),--dsp_PCOUT(N_DSPS-1),
		dsp_CARRYIN => diff_sign,
		inmode_in => "10001",
		opmode_in => "011",
		dsp_PCOUT => open, --dsp_PCOUT(0),
		dsp_P => dsp_P(0)
	);

	-- Following DSPs ------------------------------------------------------------
	DSPs :  --label name
		for i in 1 to N_DSPS-1 generate   --D DSP is instantiated N_DSPS times.
		begin  --"begin" statement for "generate"
			DSP_inst: dsp_wrapper_0 
			GENERIC MAP 
			(
				AREG_NR => 2,
				BREG_NR => 2,
				CREG_NR => 0
			)
			PORT MAP(
				CLK => CLK,
				RST => RST,
				CE => CE,
				dsp_A => dsp_ina(i),
				dsp_ACIN => zeros(29 downto 0),
				dsp_B => dsp_inb(i),
				dsp_BCIN => zeros(17 downto 0),
				dsp_C => zeros(16 downto 0) & dsp_P(0)(dsp_p_width-1 downto 17), --dsp_inc(i),
				dsp_PCIN => zeros(dsp_p_width-1 downto 0), --dsp_PCOUT(i-1), 17 bit shift puts ones to the left
				dsp_CARRYIN => '0',
				inmode_in => "10001",
				opmode_in => "011",
				dsp_PCOUT => open, --dsp_PCOUT(i),
				dsp_P => dsp_P(i)
			);
		end generate DSPs;  --end "generate" block.
	
-- End DSPs instantiation ----------------------------------------------------------------------
	
--------------------------------------------------------------------------------------------------
-- Adding 																													--
--------------------------------------------------------------------------------------------------
	
	-- Determine the sticky bit by doing the OR of the shifted bits.
	gate	:	parameterized_or_gate
	generic map
	(
		bits	=>	DISCARDED_ADDEND_WIDTH
	)
	port map
	(
		A	=>	discarded_bits,
		O	=>	sticky_bit
	);
	
	-- CHANGED -----------------------------------------------------------------------------------------------------------
	-- When adding it cannot exist overflow 
	SITUATION_1	:	if( discarded_mul_bits < RIGHTMOST_WIDTH ) generate --  MUL_RES_WIDTH > (DEF_MUL_WIDTH-RIGHTMOST_WIDTH) 
		
		RESULT_REGISTER_0 : process(CLK)
		begin
			if (rising_edge(CLK)) then
				if( RST = '1' ) then
					temp_reg0 <= (others => '0');
				elsif(CE = '1') then
					temp_reg0 <= dsp_P(0)(COL_WIDTH-1 downto 0); 
				end if;
			end if;
		end process RESULT_REGISTER_0;
	
		add_sit1_op0 <= leftmost_bits_reg3 + dsp_P(1)(31 downto 30);		
		add_res <= add_sit1_op0 & dsp_P(1)(29 downto 0) & temp_reg0(RIGHTMOST_WIDTH-1 downto discarded_mul_bits);
	
	end generate; -- END SITUATION_1
	
	SITUATION_2	:	if( discarded_mul_bits >= RIGHTMOST_WIDTH ) generate -- MUL_RES_WIDTH <= (DEF_MUL_WIDTH-RIGHTMOST_WIDTH)
		
		add_sit1_op0 <= leftmost_bits_reg3 + dsp_P(1)(31 downto 30);		
		add_res <= add_sit1_op0 & dsp_P(1)(29 downto (discarded_mul_bits-RIGHTMOST_WIDTH));
	
	end generate; -- END SITUATION_2 
	-- CHANGED -----------------------------------------------------------------------------------------------------------

	add_sign <= add_res(add_res'high);
	-- if result is negative then we have to two's complement it before nromalize the result ---------
	correct_add_res <= (NOT(add_res(norm_res_width-1 downto 0)) + 1) when add_sign = '1' else add_res(norm_res_width-1 downto 0);
	-- if different signs and w is bigger, then addition has w sign else mul_sign
	sign <= w_sign_reg3 when ((add_sign = '1') and (diff_sign_reg3 = '1')) else xy_sign_reg3; 
	
	UNNORM_RES_REGISTER: process (CLK) is
	begin
		if(rising_edge(CLK)) then
			if (RST = '1') then
				un_normalized_man_reg4 <= (others => '0');
			elsif (CE = '1') then
				un_normalized_man_reg4 <= correct_add_res;
			end if;
		end if;
	end process UNNORM_RES_REGISTER;
	
	un_normalized_man_reg5 <= un_normalized_man_reg4;
	aa_reg5 <= aa;
--------------------------------------------------------------------------------------------------
-- Normalization                                                                                --
--------------------------------------------------------------------------------------------------

	-- Prepare result to enter priority encoder -----------------------------------------------

	l_reverse: for i in un_normalized_man_reg4'range generate
		arev(un_normalized_man_reg4'high-i) <= un_normalized_man_reg4(i);
	end generate l_reverse;
	  
	-- eliminate ones past the first one
	acomp <= (not arev) + 1;
	aa <= arev and acomp;
	
	-- determine left shift value if needed to normalize add result
	-- note: the result appears to enter in this module not in the complement 2 format, but is difficult to discern
	-- why arrives here that way.
	ppe	:	one_leading 
	generic map
	(
		man_bits			=>	norm_res_width, -- 26
		shift_bits		=>	shift_width -- 5 
	)
	PORT MAP(
		MAN_IN => aa_reg5, --aa, --MAN_IN,
		SHIFT => norm_shift
	);
	
	normalizer_shifter: left_shifter
	generic map
	(
		DIN_WIDTH => norm_res_width,
		DOUT_WIDTH => norm_res_width,
		MAX_SHIFT => norm_res_width,
		SHIFT_WIDTH => shift_width
	)	
	PORT MAP(
		clk => CLK,
		rst => RST,
		ce => CE,
		din => un_normalized_man_reg5, --un_normalized_man,
		shift => norm_shift_reg6,
		sticky_bit => open,
		dout => normalized_man
	);
 
	-- Correct exponent with normalization
	-- The 55 bits comes from adjusting the result to the left, not normalization per say
	-- We have to think how to take into account adding and multiplication overflow.
	exp_after_norm <= exp_reg6 - ("0" & norm_shift_reg6);
	
--------------------------------------------------------------------------------------------------
-- Rounding                                                                                     --
--------------------------------------------------------------------------------------------------	
	
		-- should enter in the rounding module already without the hidden bit
		unrounded_res <= sign_reg7 & exp_reg7 & normalized_man(norm_res_width-2 downto 0);
	
		ROUNDING_MODULE: round_add
		generic map
		(
			exp_bits			=> exp_bits,
			man_bits_in		=> norm_res_width-1,
			man_bits_out	=>	round_man_bits
		)
		port map
		(
			--inputs
			CLK				=>	CLK,
			RESET				=> RST,
			CE					=> CE,
			STICKY_BIT_IN  => sticky_bit_reg7,
			UNRND_RES_IN	=> unrounded_res, 
			READY				=>	'0',
			EXCEPTION_IN	=>	'0', 
			--outputs
			SIGN_OUT			=> rnd_res_sign,
			EXP_OUT			=> rnd_res_exp,
			MAN_OUT			=>	rnd_res_man,
			EXCEPTION_OUT	=>	open,
			DONE				=>	open
		);
		
		rounded_res <= rnd_res_sign & rnd_res_exp & rnd_res_man;
		done <= done_reg9; -- and CE;
	
	-----------------------------------------------------------------------------------------------
	-- END: Modified if number of dsps changes ----------------------------------------------------
end Behavioral;	