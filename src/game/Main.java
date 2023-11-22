package game;

import java.awt.*;

public class Main{

    public static void main(String[] args) {
        Frame frame = new MainFrame();
        frame.setTitle("HexaGame of life");
        Image icon = Toolkit.getDefaultToolkit().getImage("hexa_icon.png");
        frame.setIconImage(icon);

        frame.setSize(700,200);
        frame.setVisible(true);
    }
}
