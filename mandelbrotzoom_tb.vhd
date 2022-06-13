library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library work;
use work.fixed_pkg.all;
USE STD.TEXTIO.ALL;
use ieee.numeric_std.all; 

entity mandelbrotzoom_tb is
--  Port ( );
end mandelbrotzoom_tb;

architecture Behavioral of mandelbrotzoom_tb is

component mandelbrotzoom is
  Port (    clk          : in std_logic;
            video_active : in std_logic;
            pixel_x      : in std_logic_vector (15 downto 0);
            pixel_y      : in std_logic_vector (15 downto 0);
            rgb          : out std_logic_vector(23 downto 0);
            ready        : out std_logic);
end component mandelbrotzoom;

signal answer_tb : sfixed(11 downto -28);
signal ready_tb : std_logic;
signal clk_tb, pixclk_tb, video_active_tb : std_logic := '0';
signal rgb_tb : std_logic_vector(23 downto 0);
signal pixel_x_tb, pixel_y_tb : std_logic_vector(15 downto 0);
file outfile : text open write_mode is "output_img.txt";
signal counter : integer := 0;

begin
uut:fixed
         port map ( clk => clk_tb,
                      video_active => video_active_tb,
                      pixel_x => pixel_x_tb,
                      pixel_y => pixel_y_tb,
                      rgb => rgb_tb,
                      ready => ready_tb);

process 
begin
clk_tb <= not clk_tb;
wait for 10ns;
end process;

--WRITE MEMORY TO FILE
--process(clk_tb)
--variable outline : line;
--begin
--if rising_edge(clk_tb) then
--    if ready_tb = '1' then 
--    write(outline,to_integer(unsigned(rgb_tb)));
--    writeline(outfile,outline);
--    counter <= counter +1;
--    end if;
--end if;
--end process;

end Behavioral;
