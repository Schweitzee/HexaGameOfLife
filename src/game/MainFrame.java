package game;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.util.Set;
import java.util.TreeSet;

import SpringUtilities.SpringUtilities;

public class MainFrame extends JFrame {
    private final JPanel MainPanel;
    private static JPanel upperPanel;
    private static JPanel lowerPanel;
    private static JTextField born;
    private static JTextField surv;
    private static JTextField tableWidth;
    private static JTextField tableHeight;
    private static JTextField hexSize;


    public MainFrame(){
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);

        MainPanel = new JPanel(new GridLayout(2,1));
        upperPanel = new JPanel(new GridLayout(1,2));

        JPanel topLeft = new JPanel(new SpringLayout());

        born = new JTextField(7);
        surv = new JTextField(7);
        tableWidth = new JTextField(7);
        tableHeight = new JTextField(7);
        hexSize = new JTextField(7);

        JButton startNewGame = new JButton("Start nem game!");
        startNewGame.addActionListener(new NewGameListener());

        topLeft.add(new JLabel("Born: "), SpringLayout.EAST);
        topLeft.add(born, SpringLayout.WEST);
        topLeft.add(new JLabel("Survive: "), SpringLayout.EAST);
        topLeft.add(surv, SpringLayout.WEST);
        topLeft.add(new JLabel("Table width: "), SpringLayout.EAST);
        topLeft.add(tableWidth, SpringLayout.WEST);
        topLeft.add(new JLabel("Table height: "), SpringLayout.EAST);
        topLeft.add(tableHeight, SpringLayout.WEST);
        topLeft.add(new JLabel("Size of hexagon (in pixel): "), SpringLayout.EAST);
        topLeft.add(hexSize, SpringLayout.WEST);
        topLeft.add(startNewGame, SpringLayout.WEST);
        topLeft.add(new JLabel(""),SpringLayout.EAST);
        SpringUtilities.makeCompactGrid(topLeft,6,2,6,6,6,6);

        JPanel topRight = new JPanel(new FlowLayout());
        JComboBox<String> sGames = new JComboBox<>(savedGames());
        JButton loadGameButton = new JButton("Load selected game!");


        topRight.add(new JLabel("Choose a saved game in the box to load it"));
        topRight.add(sGames);
        topRight.add(loadGameButton);


        upperPanel.add(topLeft);
        upperPanel.add(topRight);
        MainPanel.add(upperPanel);
        this.setLocationRelativeTo(null);
        this.add(MainPanel);
    }

    public static class NewGameListener implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            GamePanel gp = new GamePanel(Integer.parseInt(tableHeight.getText()),Integer.parseInt(tableWidth.getText()),Integer.parseInt(hexSize.getText()),makeIntSet(born.getText()),makeIntSet(surv.getText()));
            upperPanel.setVisible(false);
            lowerPanel.add(gp);
        }
    }


    /**
     * TO BE IMPLEMENTED IN MATRIX FROM FILE LOADER
     */
    public static class LoadGameListener implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            GamePanel gp = new GamePanel(_ _ _ _ _ _);
        }
    }


    public String[] savedGames(){
        File games = new File("SavedGames/");
        return games.list();
    }

    public static Set<Integer> makeIntSet(String str){
        Set<Integer> intSet = new TreeSet<>();
        char[] charSet = str.toCharArray();
        for(char c : charSet) {
            intSet.add(Integer.parseInt(Character.toString(c)));
        }
        return intSet;
    }

}
