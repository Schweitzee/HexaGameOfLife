package game;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;

import SpringUtilities.SpringUtilities;

public class MainFrame extends JFrame {
    private static final JPanel menuPanel = new JPanel(new GridLayout(1,2));
    private static final JTextField born = new JTextField(7);
    private static final JTextField surv = new JTextField(7);
    private static final JTextField tableWidth = new JTextField(7);
    private static final JTextField tableHeight = new JTextField(7);
    private static final JTextField hexSize = new JTextField(7);
    private static final JComboBox<String> sGames = new JComboBox<>(savedGames());
    public static JComboBox<String> getsGames(){return sGames;}

    public MainFrame(){
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);

        JPanel topLeft = new JPanel(new SpringLayout());

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
            int hexsize = Integer.parseInt(hexSize.getText());
            int height = Integer.parseInt(tableHeight.getText());
            int width = Integer.parseInt(tableWidth.getText());
            frame.setVisible(false);
            GameFrame gf = new GameFrame(height,width,hexsize,BasicUtil.makeIntSet(born.getText()),BasicUtil.makeIntSet(surv.getText()));
            gf.setTitle("HexaGame of life");
            Image icon = Toolkit.getDefaultToolkit().getImage("hexa_icon.png");
            gf.setIconImage(icon);
            gf.playGame(frame);
        }
    }


    public static class LoadGameListener implements ActionListener {

        JFrame frame;

        public LoadGameListener(JFrame fr) {
            frame = fr;
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            GameFrame gf;
            frame.setVisible(false);
            try {
                gf = new GameFrame(FileHandlerUtil.loadHexMatrix((String) (sGames.getSelectedItem())));
                gf.setTitle("HexaGame of life");
                Image icon = Toolkit.getDefaultToolkit().getImage("hexa_icon.png");
                gf.setIconImage(icon);
            } catch (FileNotFoundException ex) {
                System.out.println("Couldn't load game from \"" + sGames.getSelectedItem() + "\" because format is wrong.");
                frame.setVisible(true);
                throw new RuntimeException(ex);
            } catch (IOException | ClassNotFoundException ex) {
                throw new RuntimeException(ex);
            }
            gf.playGame(frame);
        }
    }

    public static String[] savedGames() {
        File games = new File("SavedGames/");
        return games.list();
    }
}
