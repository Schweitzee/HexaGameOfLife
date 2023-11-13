package game;

import javax.swing.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.IOException;
import java.time.format.DateTimeFormatter;
import java.util.Set;
import java.time.LocalDateTime;


public class GameFrame extends JFrame {

    private JFrame menufr;
    private final HexMatrix table;

    private transient DrawThread drawer;
    private transient UpdateThread updater;


    public GameFrame(int height, int width, int hexSize, Set<Integer> born, Set<Integer> alive){
        this.setSize(width*2*hexSize, height*hexSize*2);
        table = new HexMatrix(height,width, hexSize, born, alive);
        this.add(table);
        table.setSize(this.getSize());
        table.setLayout(null);
        this.setVisible(true);
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
    }


    public GameFrame(HexMatrix matrix){
        table = matrix;
        this.add(table);

    }

    public void playGame(JFrame menuframe) throws InterruptedException{
        menufr = menuframe;
        table.repaint();
        this.addKeyListener(new BasicKeyListener(this));
        this.setFocusable(true);
        drawer = new DrawThread(table, 750);
        updater = new UpdateThread(table, 750);
    }

    class BasicKeyListener implements KeyListener {
        JFrame frame;
        public BasicKeyListener(JFrame fr){
            frame = fr;
        }

        @Override
        public void keyTyped(KeyEvent e) {
            switch (e.getKeyChar()) {
                case (8): //backspace
                    updater.pause();
                    drawer.pause();
                    try {
                        updater.join();
                        drawer.join();
                    } catch (InterruptedException ex) {
                        throw new RuntimeException(ex);
                    }
                        frame.dispose();
                        menufr.setVisible(true);
                    return;
                case (' '): //space
                    updater.pause();
                    drawer.pause();
                    if (drawer.getState().equals(Thread.State.WAITING) && updater.getState().equals(Thread.State.WAITING)) {
                        drawer.notify();
                        updater.notify();
                    } else {
                        try {
                            drawer.wait();
                            updater.wait();
                        } catch (InterruptedException ex) {
                            throw new RuntimeException(ex);
                        }
                    }
                    break;
                case (39): //right arrow
                    //drawer.setSleeptime();
                    //updater.setSleeptime();
                    break;
                case (37): //left arrow
                        //drawer.setSleeptime();
                        //updater.setSleeptime();
                        break;
                default:
                    break;


            }
        }
        @Override
        public void keyPressed(KeyEvent e) {

        }

        @Override
        public void keyReleased(KeyEvent e) {

        }
    }

        class MouseListener implements java.awt.event.MouseListener{

        @Override
        public void mouseClicked(MouseEvent e) {

        }

        @Override
        public void mousePressed(MouseEvent e) {

        }

        @Override
        public void mouseReleased(MouseEvent e) {

        }

        @Override
        public void mouseEntered(MouseEvent e) {

        }

        @Override
        public void mouseExited(MouseEvent e) {

        }
    }

}
/*                    LocalDateTime dateTime = LocalDateTime.now();
                    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

                    String filename = dateTime.format(formatter) + "_" + table.getBornSet() + "-" + table.getAliveSet();

                    try {
                        FileHandlerUtil.saveHexMatrix(table, filename);
                    } catch (IOException ex) {
                        throw new RuntimeException(ex);
                    }
*/
