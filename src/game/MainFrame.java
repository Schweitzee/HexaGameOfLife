package game;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.util.HashSet;
import java.util.Set;

import SpringUtilities.SpringUtilities;

public class MainFrame extends JFrame {
    private static final JPanel menuPanel = new JPanel(new GridLayout(1,2));
    private static final JPanel gamePanel = new JPanel();
    private static JTextField born;
    private static JTextField surv;
    private static JTextField tableWidth;
    private static JTextField tableHeight;
    private static JTextField hexSize;
    private static final JComboBox<String> sGames = new JComboBox<>(savedGames());

    public MainFrame(){
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);

        this.add(gamePanel);
        gamePanel.setVisible(false);

        JPanel topLeft = new JPanel(new SpringLayout());

        born = new JTextField(7);
        surv = new JTextField(7);
        tableWidth = new JTextField(7);
        tableHeight = new JTextField(7);
        hexSize = new JTextField(7);

        JButton startNewGame = new JButton("Start nem game!");
        startNewGame.addActionListener(new NewGameListener(this));

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
        JButton loadGameButton = new JButton("Load selected game!");
        loadGameButton.addActionListener(new LoadGameListener(this));


        topRight.add(new JLabel("Choose a saved game in the box to load it"));
        topRight.add(sGames);
        topRight.add(loadGameButton);


        menuPanel.add(topLeft);
        menuPanel.add(topRight);
        this.setLocationRelativeTo(null);
        this.add(menuPanel);
    }

    public static class NewGameListener implements ActionListener {
        JFrame frame;
        public NewGameListener(JFrame fr){
            frame = fr;
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            GamePanel gp = new GamePanel(Integer.parseInt(tableHeight.getText()),Integer.parseInt(tableWidth.getText()),Integer.parseInt(hexSize.getText()),makeIntSet(born.getText()),makeIntSet(surv.getText()));
            menuPanel.setVisible(false);
            gamePanel.add(gp);
            gp.setVisible(true);
            gamePanel.setVisible(true);

            /*SIMULATION    */

            gp.setVisible(false);
            gamePanel.setVisible(false);
            menuPanel.setVisible(true);
        }
    }

    /**
     * TO BE IMPLEMENTED IN MATRIX FROM FILE LOADER
     */
    public static class LoadGameListener implements ActionListener {

        JFrame frame;

        public LoadGameListener(JFrame fr){
            frame = fr;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            try {
                GamePanel gp = new GamePanel(FileHandlerUtil.loadHexMatrix((String)(sGames.getSelectedItem())));
                menuPanel.setVisible(false);
                gamePanel.add(gp);
                gp.setVisible(true);
                gamePanel.setVisible(true);

                /*SIMULATION    */

                gp.setVisible(false);
                gamePanel.setVisible(false);
                menuPanel.setVisible(true);
            } catch (FileNotFoundException | FileHandlerUtil.FormatException ex) {
                System.out.println("Couldn't load game from \"" + sGames.getSelectedItem() + "\" because format is wrong.");
                gamePanel.setVisible(false);
                menuPanel.setVisible(true);
            }
        }
    }




















    public static String[] savedGames(){
        File games = new File("SavedGames/");
        return games.list();
    }

    public static Set<Integer> makeIntSet(String str){
        HashSet<Integer> intSet = new HashSet<>();
        char[] charSet = str.toCharArray();
        for(char c : charSet) {
            intSet.add(Integer.parseInt(Character.toString(c)));
        }
        return intSet;
    }

}
