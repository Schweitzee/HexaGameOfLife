package game;

import javax.swing.*;
import java.awt.*;

public class Main extends JFrame{
    private static final int WIDTH = 2000; // Ablak szélessége
    private static final int HEIGHT = 1000; // Ablak magassága

    public static void main(String[] args) {
        JFrame frame = new JFrame("HexaGame of life");
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        frame.setSize(WIDTH, HEIGHT);
        frame.setBackground(Color.BLACK);
        Image icon = Toolkit.getDefaultToolkit().getImage("hexa_icon.png");
        frame.setIconImage(icon);
        //frame.add(new MenuPanel());
        frame.add(new GamePanel(8, 12));
        frame.setVisible(true);
    }
}
