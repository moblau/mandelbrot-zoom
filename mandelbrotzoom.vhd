library IEEE;

use IEEE.STD_LOGIC_1164.ALL;
library work;
use work.fixed_pkg.all;
use ieee.numeric_std.all; 

entity mandelbrotzoom is
  Port (    clk          : in std_logic;
            video_active : in std_logic;
            pixel_x      : in std_logic_vector (15 downto 0);
            pixel_y      : in std_logic_vector (15 downto 0);
            rgb          : out std_logic_vector(23 downto 0);
            ready        : out std_logic);
end mandelbrotzoom;

architecture Behavioral of mandelbrotzoom is

component blk_mem_gen_0 IS
  PORT (
    clka : IN STD_LOGIC;
    ena : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(18 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
    clkb : IN STD_LOGIC;
    enb : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(18 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(5 DOWNTO 0)
  );
END component blk_mem_gen_0;

--BRAM
signal ena : STD_LOGIC;
signal wea : STD_LOGIC_VECTOR(0 DOWNTO 0);
signal addra : STD_LOGIC_VECTOR(18 DOWNTO 0):= (others => '0');
signal dina : STD_LOGIC_VECTOR(5 DOWNTO 0);
signal enb : STD_LOGIC;
signal addrb : STD_LOGIC_VECTOR(18 DOWNTO 0);
signal doutb : STD_LOGIC_VECTOR(5 DOWNTO 0);

--BRAM INDEX
signal address_ct : integer := 0;
signal address_ct_b : integer := 0;

--STATE MACHINE
type state_type is (calc_loc, assign_color, disp_v, fin);
signal state : state_type := calc_loc;

--CALCULATE PIXEL LOCATION
signal x_loc,y_loc : integer := 0;
signal x_loc_inter, y_loc_inter : sfixed(10 downto -37) := (others => '0');
signal x_scale : sfixed(10 downto -37) := (others => '0');
signal y_scale : sfixed(10 downto -37) := (others => '0');
signal x_range : sfixed(10 downto -37) := (others => '0');
signal y_range : sfixed(10 downto -37) := (others => '0');
signal color : integer := 0;
signal s_x2y2 : sfixed(10 downto -37) := (others => '0');
signal s_iter : integer := 0;
signal three, two, sixforty, foureighty : sfixed(10 downto -37);
signal four : sfixed(10 downto -37);
signal x_loc_f : sfixed(10 downto -37);
signal y_loc_f : sfixed(10 downto -37);

--BUFFER REGISTERS
signal zoom_x_inter : sfixed(10 downto -37) := ( others => '0' );
signal zoom_y_inter : sfixed(10 downto -37) := ( others => '0' );

signal zoom_iter_sf : sfixed(10 downto -37) := ( others => '0' );
signal x_scale_const : sfixed(10 downto -37) := ( others => '0' );
signal y_scale_const : sfixed(10 downto -37) := ( others => '0' );

signal x_scale_inter : sfixed(10 downto -37) := ( others => '0' );
signal y_scale_inter : sfixed(10 downto -37) := ( others => '0' );

--ESCAPE TIME ALGORITHM
signal x : sfixed(10 downto -37) := ( others => '0' );
signal y : sfixed(10 downto -37) := ( others => '0' );
signal x2 : sfixed(10 downto -37) := ( others => '0' );
signal y2 : sfixed(10 downto -37) := ( others => '0' );
signal w : sfixed(10 downto -37) := ( others => '0' );
signal iter : integer := 0;
signal x2_y2 : sfixed(10 downto -37);
signal iter2col : std_logic_vector( 7 downto 0);

--VGA OUTPUT PARAMETERS
signal pix_x, pix_y: integer;
signal start_draw : std_logic := '0';
signal draw_ct : integer := 0;
signal zoom_iter : integer := 0;

begin

bram : blk_mem_gen_0 port map ( clka => clk,
                  ena => ena,
                  wea => wea,
                  addra => addra,
                  dina => dina,
                  clkb => clk,
                  enb => enb,
                  addrb => addrb,
                  doutb => doutb);
                
                  
pix_x <= to_integer(unsigned(pixel_x));
pix_y <= to_integer(unsigned(pixel_y));


--X AND Y INCREMENT, X AND Y RANGE
zoom_iter_sf <= To_sfixed(zoom_iter,10,-37);
zoom_x_inter <= resize(To_sfixed(1249999,30,0)/To_sfixed(1000000000,30,0),zoom_x_inter);
zoom_y_inter <= resize(To_sfixed(899999,30,0)/To_sfixed(1000000000,30,0),zoom_y_inter);
x_range <= resize(To_sfixed(-2, 10,-37) + zoom_iter_sf*zoom_x_inter,x_range);
y_range <= resize(To_sfixed(1, 10,-37) - zoom_iter_sf*zoom_y_inter,y_range);
x_scale_const <= resize(To_sfixed(2999997,30,-37)/To_sfixed(1000000000,30,0),x_scale);
y_scale_const <= resize(To_sfixed(1999998,30,-37)/To_sfixed(1000000000,30,0),x_scale);
x_scale_inter <= resize(three-zoom_iter_sf*x_scale_const,x_scale);
y_scale_inter <= resize(two-zoom_iter_sf*y_scale_const,y_scale);
x_scale <= resize(x_scale_inter/sixforty,x_scale);
y_scale <= resize(y_scale_inter/foureighty,y_scale);

--FOR EASIER ARITHMETIC
four <= To_sfixed(4,10,-37);
two <= To_sfixed(2,10,-37);
three <= To_sfixed(3,10,-37);
sixforty <= To_sfixed(640,10,-37);
foureighty <= To_sfixed(480,10,-37);

--ENABLE MEMORY
ena <= '1';
enb <= '1';

--MEMORY ADDRESSES
addra <= std_logic_vector(to_signed(address_ct,addra'length));
addrb <= std_logic_vector(to_signed(address_ct_b,addra'length));

--CONVERT MEMORY OUTPUT TO 8 BIT
iter2col <= std_logic_vector(to_unsigned(to_integer(unsigned(doutb))*4-1,iter2col'length));

process(clk)
begin
    if rising_edge(clk) then
        case state is
            --CALCULATE PIXEL LOCATION
            when calc_loc =>
                wea <= "1";
                ready <= '0';
                x_loc_f <= To_sfixed(x_loc,10,-37);
                y_loc_f <= To_sfixed(y_loc,10,-37);
                x_loc_inter <= resize(x_scale*x_loc_f+x_range,x_loc_inter);
                y_loc_inter <= resize(y_range-y_scale*y_loc_f,y_loc_inter);
                x_loc <= x_loc + 1;
                iter <= 0;
                x2 <= (others => '0');
                y2 <= (others => '0');
                x <= (others => '0');
                y <= (others => '0');
                w <= (others => '0');
                state <= assign_color;
                if x_loc = 639 and y_loc = 479 then
                    state<= disp_v;
                elsif x_loc = 639 then
                    x_loc <= 0;
                    y_loc <= y_loc+1;
                end if;
            --ESCAPE TIME ALGORITHM
            when assign_color =>
                if resize(x2+y2,x2) < four and iter < 64 then               
                    x <= resize(x2 - y2 + x_loc_inter,y_loc_inter);
                    y <= resize(w - x2 - y2 + y_loc_inter,y_loc_inter);
                    x2 <= resize(x*x,y_loc_inter);
                    y2 <= resize(y*y,y_loc_inter);
                    w <= resize((x + y)*(x + y),y_loc_inter);
                    iter <= iter + 1;
                else 
                    address_ct <= address_ct+1;               
                    color <= iter;
                    dina <= std_logic_vector(to_signed(color,dina'length));
                    s_x2y2 <= x2_y2;
                    s_iter <= iter;
                    state <= calc_loc;    
                end if;  
            --DELAY STATE MACHINE TO DISPLAY VIDEO         
            when disp_v =>
                wea <= "0";
                draw_ct <= draw_ct+1;
                if draw_ct = 1000 then
                    state <= calc_loc;
                    zoom_iter <= zoom_iter + 1;
                    y_loc <= 0;
                    x_loc <= 0;
                    address_ct <= 0;
                    start_draw <= '0';
                    draw_ct <= 0;
                    if zoom_iter = 999 then
                        zoom_iter <= 0;
                    end if;
                end if;
            when fin =>
                ready <= '0';
            when others =>
            end case;
    end if;
end process;

--PIXEL DATA TO VGA
process(clk)
begin
if rising_edge(clk) then
    if video_active = '1' then 
        address_ct_b <= address_ct_b+1;
        rgb <= iter2col & iter2col & iter2col;                        
        if address_ct_b = 307199 then
            address_ct_b <= 0;                      
        end if; 
    end if;
end if;
end Behavioral;
