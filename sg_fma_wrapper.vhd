----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    17:06:02 05/15/2014 
-- Design Name: 
-- Module Name:    sg_man_mul_wrapper - Behavioral 
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

entity sg_fma_wrapper is generic
(
	OPX_WIDTH : integer 		:= 32;
	OPY_WIDTH : integer 		:= 32;
	OPW_WIDTH : integer 		:= 32;
	MUL_RES_WIDTH : integer := 48;
	OP_RES_WIDTH : integer 	:= 32
);
port(
	CLK			: in 	std_logic;
	RST			: in 	std_logic;
	CE 			: in 	std_logic;
	INIT_CALC	: in 	std_logic;
	WRITE2FILE	: in 	std_logic;
	X : in std_logic_vector(OPX_WIDTH-1 downto 0);
	Y : in std_logic_vector(OPY_WIDTH-1 downto 0);
	W : in std_logic_vector(OPW_WIDTH-1 downto 0);
	RES : out std_logic_vector(OP_RES_WIDTH-1 downto 0);
	DONE : out std_logic
);
end sg_fma_wrapper;

architecture Behavioral of sg_fma_wrapper is

--	COMPONENT write_file
--	GENERIC 
--	(
--		DATA_WIDTH 			: integer := 106
--	);
--	PORT(
--		clk : IN std_logic;
--		data_in : IN std_logic_vector(DATA_WIDTH-1 downto 0);
--		we : IN std_logic   
--		);
--	END COMPONENT;
	
	constant sg_man_width	 	: integer := 23;
	constant sg_norm_man_width	: integer := sg_man_width+1;
	constant sg_exp_width		: integer := 8;
	
	constant sg_man_width_w	 		: integer := OPW_WIDTH-sg_exp_width-1;
	constant sg_norm_man_width_w	: integer := sg_man_width_w+1;
	
	constant sg_mul_res_width	: integer := MUL_RES_WIDTH;
	constant default_mul_width : integer := 48;
	constant discarded_mul_bits: integer := default_mul_width-sg_mul_res_width;
	
	constant pre_shifter_width : integer := sg_mul_res_width + 2*sg_norm_man_width_w + 1; -- sg_mul_res_width + 2*sg_norm_man_width_w + 1  = 213 (tops)
	constant discarded_bits_width : integer := sg_norm_man_width_w; -- 53
	constant addend_width 		: integer := sg_mul_res_width + sg_norm_man_width_w + 1;-- - discarded_bits_width; --160;
	-- one bit because of the round bit and another because to align we have to take into account the mul overflow
	constant w_init_align		: integer := sg_norm_man_width_w + 2; 
	
	constant exp_bias 			: integer := 127;
	constant zero_exponent		: std_logic_vector(sg_exp_width-1 downto 0) := (others => '0');
	constant shift_width 		: integer := 7; -- maximum shift is 73 bits
	
	signal dsp_ina : dsp_ina_type;
	signal dsp_inb : dsp_inb_type;
	signal dsp_inc : dsp_inc_type;
	
	--Auxiliary signals
	signal done_reg0, done_reg1 : std_logic;
	
	signal x0 : std_logic_vector(16 downto 0);
	signal x1 : std_logic_vector(6 downto 0);
	signal y0 : std_logic_vector(23 downto 0);
	
	signal w_man : std_logic_vector(sg_norm_man_width_w-1 downto 0);
	
	signal x_exp, y_exp : std_logic_vector(sg_exp_width-1 downto 0);
	signal xy_exp, xy_exp_reg0, xy_exp_corr, xy_exp_corr_reg0, xy_exp_corr_reg1 : std_logic_vector(sg_exp_width-1 downto 0);
	signal w_new_exp, w_exp, w_exp_reg0, w_exp_corr, w_exp_corr_reg1 : std_logic_vector(sg_exp_width-1 downto 0);
	signal mux_exp_reg0, mux_exp_reg1 : std_logic;
	signal add_exp, add_exp_reg2 : std_logic_vector(sg_exp_width-1 downto 0);
	-- one more bit needed for the signal
	signal exp_diff : std_logic_vector(sg_exp_width downto 0);
	signal exp_diff_new, exp_diff_new_reg0 : std_logic_vector(sg_exp_width downto 0);
	signal shift_amount, shift_amount_reg0 : std_logic_vector(shift_width-1 downto 0);
	signal zeros : std_logic_vector(shift_width-1 downto 0);
	
	signal leftmost_bits : std_logic_vector(addend_width-sg_mul_res_width downto 0);
	signal aligned_with_xy : std_logic_vector(default_mul_width-1 downto 0);
	signal pad_mul_bits : std_logic_vector(discarded_mul_bits-1 downto 0);
	signal addend : std_logic_vector(addend_width downto 0);
--	signal sticky_bit_tmp : std_logic;
	signal discarded_bits_tmp : std_logic_vector(discarded_bits_width-1 downto 0);
	
   signal done_tmp : std_logic;
	
	signal X_REG : std_logic_vector(OPX_WIDTH-1 downto 0);
	signal Y_REG : std_logic_vector(OPY_WIDTH-1 downto 0);
	signal W_REG : std_logic_vector(OPW_WIDTH-1 downto 0);
	
	signal X_REG2 : std_logic_vector(OPX_WIDTH-1 downto 0);
	signal Y_REG2 : std_logic_vector(OPY_WIDTH-1 downto 0);
	
	signal x_sign, y_sign, w_sign, mul_sign : std_logic;
	signal diff_signs, diff_signs_reg0, diff_signs_reg1 : std_logic;
	signal mul_sign_reg0, mul_sign_reg1 : std_logic;
	signal w_sign_reg0, w_sign_reg1: std_logic;
	signal w_notzero, w_notzero_reg0, w_notzero_reg1 : std_logic;
	
	signal rounded_res : std_logic_vector(OP_RES_WIDTH-1 downto 0);
	signal WRITE2FILE_REG : std_logic;
	signal READY : std_logic;
	
	signal x_notzero, y_notzero, mul_res_not_zero, mul_res_not_zero_reg0, mul_res_not_zero_reg1 : std_logic;
	signal xy_exp_temp : std_logic_vector(sg_exp_width-1 downto 0);
	
begin

	zeros <= (others => '0');
	---------------------------------------------------------------------------------
	-- Input Data -------------------------------------------------------------------
	---------------------------------------------------------------------------------
--	INPUT_REGS: process (CLK) is
--	begin
--		if(rising_edge(CLK)) then
--			if (CE = '1') then
--				X_REG <= X;
--				Y_REG <= Y;
--				W_REG <= W; --X"11111111"; --
--				READY <= INIT_CALC; 
--				WRITE2FILE_REG <= WRITE2FILE;
--			end if;
--		end if;
--	end process INPUT_REGS;
	
	X_REG <= X;
	Y_REG <= Y;
	W_REG <= W; -- X"11111111"; --
	READY <= INIT_CALC;
			
	-- Simulation Only ---------------------
--	ADD_OP_REGS: process (CLK) is
--	begin
--		if(rising_edge(CLK)) then
--			if( RST = '1' ) then 
--				W_REG <= (others => '0');
--			else
--				if( WRITE2FILE_REG = '1' ) then
--					W_REG <= rounded_res;
--				end if;
--			end if;
--		end if;
--	end process ADD_OP_REGS;	
	
--	X_REG <= X;
--	Y_REG <= Y;
--	W_REG <= W;
--	W_REG <= rounded_res; -- accumulation only
--	WRITE2FILE_REG <= WRITE2FILE; -- simulation only
	
	-- !!!ATENTION: We have yet to take into account the possibility of X or Y being zero and thus the multiplication result too.
	detect_zero_X	:	parameterized_or_gate generic map( bits	=>	sg_exp_width) port map(A	=>	x_exp, O	=>	x_notzero);
	detect_zero_Y	:	parameterized_or_gate generic map( bits	=>	sg_exp_width) port map(A	=>	y_exp, O	=>	y_notzero);
	detect_zero_W	:	parameterized_or_gate generic map( bits	=>	sg_exp_width) port map(A	=>	w_exp, O	=>	w_notzero);
	
	x0 <= X_REG(16 downto 0);
	x1 <= x_notzero & X_REG(22 downto 17);
	x_exp <= X_REG(30 downto 23);
	x_sign <= X_REG(31);
	y0 <= y_notzero & Y_REG(22 downto 0);
	y_exp <= Y_REG(30 downto 23);
	y_sign <= Y_REG(31);
	
	w_exp <= W_REG(OPW_WIDTH-2 downto sg_man_width_w);
	w_sign <= W_REG(OPW_WIDTH-1);
	
	w_man <= w_notzero & W_REG(sg_man_width_w-1 downto 0);
	
	-- EXPONENT HANDLING --------------------------------------------------------------------------------
	mul_res_not_zero <= x_notzero and y_notzero;
	xy_exp_temp <= x_exp + y_exp - exp_bias;
	xy_exp <= xy_exp_temp when mul_res_not_zero = '1' else zero_exponent;
	w_new_exp <= w_exp - w_init_align;
	exp_diff <= ('0' & xy_exp) - ('0' & w_exp);
	exp_diff_new <= ('0' & xy_exp) - ('0' & w_new_exp);
	
	-- Determining shift amount
	shift_amount <= zeros when exp_diff_new_reg0(exp_diff_new'high) = '1' else exp_diff_new_reg0(shift_width-1 downto 0);
		
--	mux_exp <= exp_diff(exp_diff'high);
	-- Must correct exponent because the result is aligned to the left, thus we compensate the normalization shift
	w_exp_corr <= w_exp_reg0 + ("0" & shift_amount); --_reg0
	xy_exp_corr <= xy_exp + w_init_align;
	
	add_exp <= w_exp_corr_reg1 when ( (mux_exp_reg1 = '1' and w_notzero_reg1 = '1') or (mul_res_not_zero_reg1 = '0') ) else xy_exp_corr_reg1; -- w_new_exp
	
	-- SIGN HANDLING -----------------------------------------------------------------------------------
	mul_sign <= x_sign XOR y_sign;
	diff_signs <= (mul_sign XOR w_sign) and w_notzero;
	
	---------------------------------------------------------------------------------
	-- REGISTERS 
	---------------------------------------------------------------------------------
	REGS: process (CLK) is
	begin
		if(rising_edge(CLK)) then
			if (RST = '1') then
				-- First Stage
				exp_diff_new_reg0 <= (others => '0');
				w_exp_reg0 <= (others => '0');
				xy_exp_corr_reg0 <= (others => '0');
				mux_exp_reg0 <= '0';
				w_notzero_reg0 <= '0';
				
				mul_sign_reg0 <= '0';
				w_sign_reg0 <= '0';
				diff_signs_reg0 <= '0';
				
				done_reg0 <= '0';
				mul_res_not_zero_reg0 <= '0';
				
				-- Second Stage
				w_exp_corr_reg1 <= (others => '0');
				xy_exp_corr_reg1 <= (others => '0');
				mux_exp_reg1 <= '0';
				w_notzero_reg1 <= '0';
				
				mul_sign_reg1 <= '0';
				w_sign_reg1 <= '0';
				diff_signs_reg1 <= '0';
				
				done_reg1 <= '0';
				mul_res_not_zero_reg1 <= '0';
				-- Third stage
				add_exp_reg2 <= (others => '0');
			elsif (CE = '1') then
				-- First Stage
				exp_diff_new_reg0 <= exp_diff_new;
				w_exp_reg0 <= w_exp;
				xy_exp_corr_reg0 <= xy_exp_corr;
				mux_exp_reg0 <= exp_diff(exp_diff'high);
				w_notzero_reg0 <= w_notzero;
				
				mul_sign_reg0 <= mul_sign;
				w_sign_reg0 <= w_sign;
				diff_signs_reg0 <= diff_signs;
				
				done_reg0 <= READY;
				mul_res_not_zero_reg0 <= mul_res_not_zero;
				
				-- Second Stage
				w_exp_corr_reg1 <= w_exp_corr;
				xy_exp_corr_reg1 <= xy_exp_corr_reg0;
				mux_exp_reg1 <= mux_exp_reg0;
				w_notzero_reg1 <= w_notzero_reg0;
				
				mul_sign_reg1 <= mul_sign_reg0;
				w_sign_reg1 <= w_sign_reg0;
				diff_signs_reg1 <= diff_signs_reg0;
				
				done_reg1 <= done_reg0;
				mul_res_not_zero_reg1 <= mul_res_not_zero_reg0;
				-- Third stage
				add_exp_reg2 <= add_exp;
			end if;
		end if;
	end process REGS;
	
	---------------------------------------------------------------------------------
	-- Alignment done with right shifter							  					       --
	---------------------------------------------------------------------------------
	pre_shifter: right_shifter
	generic map
	(
		DIN_WIDTH => sg_norm_man_width_w,
		DATA_WIDTH => pre_shifter_width,
		DOUT_WIDTH => addend_width,
		MAX_SHIFT => addend_width,
		SHIFT_WIDTH => shift_width,
		DISCARDED_WIDTH => discarded_bits_width
	)
	PORT MAP(
		clk => CLK,
		rst => RST,
		ce  => CE,
		din => w_man,
		shift => shift_amount,
		sign => diff_signs_reg1,
		signreg => diff_signs_reg1,
--		sticky_bit => sticky_bit_tmp,
		discarded_bits => discarded_bits_tmp,
		dout => addend
	);
	
	---------------------------------------------------------------------------------
	
	leftmost_bits <= addend(addend_width downto sg_mul_res_width);
	pad_mul_bits <= (others => '0'); --(others => '1') when diff_signs_reg2 = '1' else (others => '0');
	aligned_with_xy <= addend(sg_mul_res_width-1 downto 0) & pad_mul_bits;	
	
	---------------------------------------------------------------------------------
	-- DSPs input values 																	       --
	---------------------------------------------------------------------------------	
	dsp_inc(0) <= "0" & aligned_with_xy(46 downto 0);
	dsp_inb(0) <= "0" & x0;
	dsp_ina(0) <= "000000" & y0;
	-- must account for the overflow
	dsp_inc(1) <= (others => '0');
	dsp_inb(1) <= "00000000000" & x1;
	dsp_ina(1) <= "000000" & y0;
	
	---------------------------------------------------------------------------------
	-- DSPs instantiations 																			 --
	---------------------------------------------------------------------------------
	Inst_sg_fma: sg_fma
	GENERIC MAP
	(
		DISCARDED_ADDEND_WIDTH => discarded_bits_width,
		MUL_RES_WIDTH => sg_mul_res_width,
		DEF_MUL_WIDTH => default_mul_width,
		RIGHTMOST_WIDTH => 17,
		LEFTMOST_WIDTH => addend_width - sg_mul_res_width + 1 + 1, -- more one bit aligned with mul result
		ADDITION_WIDTH => addend_width + 1,
		COL_WIDTH => 17,
		NB_COL => 5,
		EXP_WIDTH => 8,
		RES_WIDTH => OP_RES_WIDTH
	)
	PORT MAP(
		CLK => CLK,
		RST => RST,
		CE => CE,
		ready => done_reg1,
		dsp_ina => dsp_ina,
		dsp_inb => dsp_inb,
		dsp_inc => dsp_inc,
		leftmost_bits => leftmost_bits & aligned_with_xy(default_mul_width-1),
		discarded_bits => discarded_bits_tmp,
		xy_sign => mul_sign_reg1, 
		w_sign => w_sign_reg1,
		diff_sign => diff_signs_reg1,
		exp => add_exp_reg2,
		rounded_res => rounded_res,
		done => done_tmp
	);
	
	RES <= rounded_res;
	---------------------------------------------------------------------------------
	-- Simulation Only 																			 --
	---------------------------------------------------------------------------------
	
--	WRITE_TO_FILE: write_file
--	GENERIC MAP
--	(
--		DATA_WIDTH => OP_RES_WIDTH
--	)
--	PORT MAP 
--	(
--      clk => CLK,
--      data_in => rounded_res, --(sg_man_width-1 downto 0),
--      we => WRITE2FILE_REG
--   );
	
	---------------------------------------------------------------------------------
	
	DONE <= done_tmp;

end Behavioral;

