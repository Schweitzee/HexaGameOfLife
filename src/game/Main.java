package game;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class Main{
    private static final int WIDTH = Toolkit.getDefaultToolkit().getScreenSize().width; // Ablak szélessége
    private static final int HEIGHT = Toolkit.getDefaultToolkit().getScreenSize().height; // Ablak magassága

    public static void main(String[] args) {
        Frame frame = new MainFrame();
        frame.setTitle("HexaGame of life");
        Image icon = Toolkit.getDefaultToolkit().getImage("hexa_icon.png");
        frame.setIconImage(icon);

        frame.setSize(700,200);
        frame.setVisible(true);
    }
}
