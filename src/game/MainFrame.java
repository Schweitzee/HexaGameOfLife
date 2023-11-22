package game;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;

import SpringUtilities.SpringUtilities;


/**
 * MainFrame is the first frame that appears when the game is launched.
 * It allows the user to choose between starting a new game or loading a saved one.
 * It also allows the user to choose the size of the hexagons, the size of the table and the rules of the game.
 */
public class MainFrame extends JFrame {

    /**
     * menuPanel is the panel that contains the different components of the menu.
     */
    private static final JPanel menuPanel = new JPanel(new GridLayout(1,2));

    /**
     * born is the text field where the user can enter the numbers of neighbours that make a dead cell become alive.
     */
    private static final JTextField born = new JTextField(7);

    /**
     * surv is the text field where the user can enter the numbers of neighbours that make a living cell stay alive.
     */
    private static final JTextField surv = new JTextField(7);

    /**
     * tableWidth is the text field where the user can enter the width of the table.
     */
    private static final JTextField tableWidth = new JTextField(7);

    /**
     * tableHeight is the text field where the user can enter the height of the table.
     */
    private static final JTextField tableHeight = new JTextField(7);

    /**
     * hexSize is the text field where the user can enter the size of the hexagons.
     */
    private static final JTextField hexSize = new JTextField(7);

    /**
     * sGames is the combo box where the user can choose a saved game.
     */
    private static final JComboBox<String> sGames = new JComboBox<>(savedGames());

    /**
     * Getter for the combo box to be able to update it when a new game is saved.
     * @return the combo box where the user can choose a saved game.
     */
    public static JComboBox<String> getsGames(){return sGames;}


    /**
     * Constructor of the MainFrame class.
     * It creates the frame and adds the different components and listeners to it.
     */
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

    /**
     * NewGameListener handles the event when the user clicks on the "Start new game!" button.
     * It creates a new GameFrame with the parameters entered by the user.
     * Continues by calling the playGame method of the GameFrame class.
     */
    public static class NewGameListener implements ActionListener {

        /**
         * numbers is the regex that matches any number between 0 and 9.
         * It is used to check if the user entered only numbers in the text fields.
         */
        private String numbers = "[0-9]+";

        /**
         * Frame that contains the menu to be able to hide it when the game starts and show it again when the game ends.
         */
        JFrame frame;

        /**
         * Constructor of the NewGameListener class.
         * @param fr the frame that contains the menu to be able to hide it when the game starts and show it again when the game ends.
         */
        public NewGameListener(JFrame fr){
            frame = fr;
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            if (hexSize.getText().matches(numbers) &&
                    born.getText().matches(numbers) &&
                    surv.getText().matches(numbers) &&
                    tableHeight.getText().matches(numbers) &&
                    tableWidth.getText().matches(numbers)) {
                int hexsize = Integer.parseInt(hexSize.getText());
                int height = Integer.parseInt(tableHeight.getText());
                int width = Integer.parseInt(tableWidth.getText());
                frame.setVisible(false);
                GameFrame gf = new GameFrame(height, width, (Math.max(hexsize, 5)), BasicUtil.makeIntSet(born.getText()), BasicUtil.makeIntSet(surv.getText()));
                gf.setTitle("HexaGame of life");
                Image icon = Toolkit.getDefaultToolkit().getImage("hexa_icon.png");
                gf.setIconImage(icon);
                gf.playGame(frame);
            }else {
                JOptionPane.showMessageDialog(frame, "Please enter only numbers in the fields.");
            }
        }
    }

    /**
     * LoadGameListener handles the event when the user clicks on the "Load selected game!" button.
     * It loads the game selected by the user and continues by calling the playGame method of the GameFrame class.
     */
    public static class LoadGameListener implements ActionListener {

        /**
         * Frame that contains the menu to be able to hide it when the game starts and show it again when the game ends.
         */
        JFrame frame;

        /**
         * Constructor of the LoadGameListener class.
         * @param fr the frame that contains the menu to be able to hide it when the game starts and show it again when the game ends.
         */
        public LoadGameListener(JFrame fr) {
            frame = fr;
        }

        /**
         * actionPerformed loads the game selected by the user and continues by calling the playGame method of the GameFrame class.
         * @param e the event that triggered the listener.
         */
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

    /**
     * savedGames returns the list of the saved games in the SavedGames folder.
     * @return the list of the saved games.
     */
    public static String[] savedGames() {
        File games = new File("SavedGames/");
        return games.list();
    }
}
