package game;

import javax.swing.*;
import java.awt.event.*;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Set;


/**
 * The GameFrame class is responsible for the game window.
 * The game window displays the game and controls the inputs manipulating the game.
 */
public class GameFrame extends JFrame {

    /**
     * The menu frame that is displayed when the game is ended.
     */
    private JFrame menufr;

    /**
     * The matrix that is displayed in the window.
     */
    private final HexMatrix table;

    /**
     * The thread that is responsible for drawing the game.
     */

    private transient DrawThread drawer;

    /**
     * The thread that is responsible for updating the game.
     */
    private transient UpdateThread updater;

    /**
     * The size of the hexagons.
     */
    private final int hexSize;

    /**
     * The enum that represents the speed of the game.
     */
    private SleepState slSt = SleepState.NORMAL;


    /**
     * The constructor of the GameFrame class.
     * @param height The height of the game board.
     * @param width The width of the game board.
     * @param hexSize The size of the hexagons.
     * @param born The set of numbers of neighbours that a dead cell needs to be born.
     * @param alive The set of numbers of neighbours that a living cell needs to stay alive.
     */
    public GameFrame(int height, int width, int hexSize, Set<Integer> born, Set<Integer> alive){
        this.setSize((int)(width*1.6*hexSize), (int)(height*hexSize*2));
        table = new HexMatrix(height,width, hexSize, born, alive);
        this.addWindowListener(new WindowAdapter());

        this.hexSize = hexSize;
        this.add(table);

        table.setLayout(null);
        this.setVisible(true);
    }


    /**
     * The constructor of the GameFrame class that is used when a game is loaded from a file.
     * @param matrix The HexMatrix object that is displayed in the window.
     */
    public GameFrame(HexMatrix matrix){
        this.hexSize = matrix.getHexSize();
        this.setSize((int)(matrix.getWi()*1.6*hexSize), (int)(matrix.getHe()*hexSize*2));

        this.addWindowListener(new WindowAdapter());
        table = matrix;
        this.add(table);

        table.setLayout(null);
        this.setVisible(true);
    }

    class WindowAdapter extends java.awt.event.WindowAdapter {
        /**
         * This function is called when the window is closed, and is responsible for stopping the threads, disposing the window and displaying the menu window.
         * @param e The window event.
         */
        @Override
        public void windowClosing(WindowEvent e) {escaper();}
    }

    /**
     * This function is  called when the game is paused and the esc key is pressed to return to the menu.
     * Stops the threads, disposes the game window and displays the menu window.
     */
    private void escaper() {
        if(Boolean.TRUE.equals(table.getPaused())){
            table.setPaused(false);
            table.noti();
        }
        updater.end();
        drawer.end();

        try {
            updater.join();
            drawer.join();
        } catch (InterruptedException ex) {
            throw new RuntimeException(ex);
        }
        this.dispose();
        menufr.repaint();
        menufr.setVisible(true);
    }



    /**
     * Funtion that starts the game and sets the listeners for the game.
     * @param menuframe The menu frame that is displayed again when the game is paused.
     */
    public void playGame(JFrame menuframe){
        menufr = menuframe;
        table.repaint();
        table.addMouseListener(new MouseListener());
        this.addKeyListener(new BasicKeyListener(this));
        this.setFocusable(true);
        drawer = new DrawThread(table, 750);
        updater = new UpdateThread(table, 750);
        drawer.start();
        updater.start();
    }

    /**
     * This class is responsible for the key inputs.
     */
    class BasicKeyListener implements KeyListener {

        /**
         * The frame that is displayed when the game is paused.
         */
        JFrame frame;
        /**
         * Constructor of the BasicKeyListener class.
         * @param fr The frame that is displayed when the game is paused.
         */
        public BasicKeyListener(JFrame fr){
            frame = fr;
        }

        /**
         * This function is responsible for the key typed events.
         * @param e The key event.
         */
        @Override
        public void keyTyped(KeyEvent e) {
            switch (e.getKeyChar()) {
                case (' '): //space to pause or resume
                    if(Boolean.TRUE.equals(table.getPaused())) {
                        table.setDrawableTrue();
                        table.setPaused(false); //to resume the threads
                        table.noti(); //to notify the threads
                    }else if(Boolean.FALSE.equals(table.getPaused())){
                        table.setPaused(true); //to stop the threads
                        table.repaint(); //to draw the paused screen
                    }
                    break;
                case ('\n'): //enter to save
                    if(Boolean.TRUE.equals(table.getPaused())) {
                        //the format of the file name is the current date and time and the born and alive sets
                        LocalDateTime currentDateTime = LocalDateTime.now().truncatedTo(java.time.temporal.ChronoUnit.SECONDS);
                        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd_HH:mm:ss");
                        String formattedDateTime = currentDateTime.format(formatter).replace(":", "_");
                        try {
                            FileHandlerUtil.saveHexMatrix(table, formattedDateTime + "_" + table.getBornSet() + "-" + table.getAliveSet());
                        } catch (IOException ex) {
                            throw new RuntimeException(ex);
                        }
                        //adds the name of the saved game to the list of saved games
                        MainFrame.getsGames().addItem(formattedDateTime + "_" + table.getBornSet() + "-" + table.getAliveSet());
                    }
                        break;
                default:
                    break;


            }
        }


        /**
         * This function is called when a key is pressed, and is responsible for the speed up and slow down of the game.
         */
        @Override
        public void keyPressed(KeyEvent e) {
            if (e.getKeyCode() == KeyEvent.VK_RIGHT) {
                faster();
            }else if(e.getKeyCode() == KeyEvent.VK_LEFT){
                slower();
            }
        }

        /**
         * This function is called when a key is released, and is responsible for the esc key to return to the menu.
         */
        @Override
        public void keyReleased(KeyEvent e) {
            if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
                escaper();
            }
        }
    }

    /**
     * This class is responsible for the mouse inputs.
     * It is used to change the state of the hexagons when the game is paused.
     */
    class MouseListener implements java.awt.event.MouseListener {

        /**
         * Constructor of the MouseListener class.
         */
        public MouseListener() {}


        /**
         * This function is called a mouse click is detected. If the game is paused, it changes the state of the hexagon that was clicked.
         * @param e The mouse event.
         */
        @Override
        public void mouseClicked(MouseEvent e) {
            if (Boolean.TRUE.equals(table.getPaused())) {
                int x = e.getX();
                int y = e.getY();
                Hex[][] hexes = table.getHexes();
                for (Hex[] row : hexes) {
                    for (Hex hex : row) {
                        if (Boolean.TRUE.equals(hex.contains(x, y))) {
                            hex.setNewState(!Boolean.TRUE.equals(hex.getState()));
                            hex.switchStateToNewState();
                            table.repaint();
                            return;
                        }
                    }
                }
            }
        }

        /**
         * Unused function
         */
        @Override
        public void mousePressed(MouseEvent e) {}

        /**
         * Unused function
         */
        @Override
        public void mouseReleased(MouseEvent e) {}

        /**
         * Unused function
         */
        @Override
        public void mouseEntered(MouseEvent e) {}

        /**
         * Unused function
         */
        @Override
        public void mouseExited(MouseEvent e) {}
    }


    /**
     * This function is called when the right arrow key is pressed, and is responsible setting the speed of the game to faster.
     */
    public void faster(){
        switch (slSt){
            case SLOWEST:
                slSt = SleepState.SLOW;
                updater.setSleeptime(750);
                drawer.setSleeptime(750);
                break;
            case SLOW:
                slSt = SleepState.NORMAL;
                updater.setSleeptime(500);
                drawer.setSleeptime(500);
                break;
            case NORMAL:
                slSt = SleepState.FAST;
                updater.setSleeptime(350);
                drawer.setSleeptime(350);
                break;
            case FAST:
                slSt = SleepState.FASTER;
                updater.setSleeptime(200);
                drawer.setSleeptime(200);
                break;
            case FASTER:
                slSt = SleepState.FASTEST;
                updater.setSleeptime(100);
                drawer.setSleeptime(100);
                break;
            default:
                break;
        }
    }

    /**
     * This function is called when the left arrow key is pressed, and is responsible setting the speed of the game to slower.
     */
    public void slower() {
        switch (slSt) {
            case SLOW:
                slSt = SleepState.SLOWEST;
                updater.setSleeptime(1000);
                drawer.setSleeptime(1000);
                break;
            case NORMAL:
                slSt = SleepState.SLOW;
                updater.setSleeptime(750);
                drawer.setSleeptime(750);
                break;
            case FAST:
                slSt = SleepState.NORMAL;
                updater.setSleeptime(500);
                drawer.setSleeptime(500);
                break;
            case FASTER:
                slSt = SleepState.FASTEST;
                updater.setSleeptime(350);
                drawer.setSleeptime(350);
                break;
            case FASTEST:
                slSt = SleepState.FAST;
                updater.setSleeptime(200);
                drawer.setSleeptime(200);
                break;
            default:
                break;
        }
    }
}
