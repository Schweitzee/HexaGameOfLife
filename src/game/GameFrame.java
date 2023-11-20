package game;

import javax.swing.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Set;


public class GameFrame extends JFrame {

    private JFrame menufr;
    private final HexMatrix table;

    private transient DrawThread drawer;
    private transient UpdateThread updater;

    private final int hexSize;

    private SleepState slSt = SleepState.NORMAL;


    public GameFrame(int height, int width, int hexSize, Set<Integer> born, Set<Integer> alive){
        this.setSize((int)(width*1.7*hexSize), (int)(height*hexSize*2.1));
        table = new HexMatrix(height,width, hexSize, born, alive);
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
        this.hexSize = hexSize;
        this.add(table);
        table.setSize(this.getSize());
        table.setLayout(null);
        this.setVisible(true);
    }


    public GameFrame(HexMatrix matrix){
        this.hexSize = matrix.getHexSize();
        this.setSize((int)(matrix.getWi()*1.7*hexSize), (int)(matrix.getHe()*hexSize*2.1));
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
        table = matrix;
        this.add(table);
        table.setSize(this.getSize());
        table.setLayout(null);
        this.setVisible(true);
    }

    public void playGame(JFrame menuframe){
        menufr = menuframe;
        table.repaint();
        table.addMouseListener(new MouseListener(this));
        this.addKeyListener(new BasicKeyListener(this));
        this.setFocusable(true);
        drawer = new DrawThread(table, 750);
        updater = new UpdateThread(table, 750);
        drawer.start();
        updater.start();
    }

    class BasicKeyListener implements KeyListener {
        JFrame frame;
        public BasicKeyListener(JFrame fr){
            frame = fr;
        }

        @Override
        public void keyTyped(KeyEvent e) {
            switch (e.getKeyChar()) {
                case (' '): //space
                    if(Boolean.TRUE.equals(table.getPaused())) {
                        table.setDrawableTrue();
                        table.setPaused(false);
                        table.noti();
                    }else if(Boolean.FALSE.equals(table.getPaused())){
                        table.setPaused(true);
                        table.repaint();
                    }
                    break;
                case (10): //enter
                    if(Boolean.TRUE.equals(table.getPaused())) {
                        LocalDateTime currentDateTime = LocalDateTime.now().truncatedTo(java.time.temporal.ChronoUnit.SECONDS);
                        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd_HH:mm:ss");
                        String formattedDateTime = currentDateTime.format(formatter).replace(":", "_");

                        try {
                            FileHandlerUtil.saveHexMatrix(table, formattedDateTime + "_" + table.getBornSet() + "-" + table.getAliveSet());
                        } catch (IOException ex) {
                            throw new RuntimeException(ex);
                        }
                        MainFrame.setsGames();
                        escaper();
                    }
                        break;
                default:
                    break;


            }
        }

        private void escaper() {
            updater.end();
            drawer.end();
            try {
                updater.join();
                drawer.join();
            } catch (InterruptedException ex) {
                throw new RuntimeException(ex);
            }
            frame.dispose();
            menufr.setVisible(true);
        }

        @Override
        public void keyPressed(KeyEvent e) {
            if (e.getKeyCode() == KeyEvent.VK_RIGHT) {
                faster();
            }else if(e.getKeyCode() == KeyEvent.VK_LEFT){
                slower();
            }
        }
        @Override
        public void keyReleased(KeyEvent e) {
            if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
                escaper();
            }
        }
    }

    class MouseListener implements java.awt.event.MouseListener {
        JFrame frame;

        public MouseListener(JFrame fr) {
            frame = fr;
        }

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

        @Override
        public void mousePressed(MouseEvent e) {
            //Unused
        }

        @Override
        public void mouseReleased(MouseEvent e) {
            //Unused
        }

        @Override
        public void mouseEntered(MouseEvent e) {
            //Unused
        }

        @Override
        public void mouseExited(MouseEvent e) {
            //Unused
        }
    }

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