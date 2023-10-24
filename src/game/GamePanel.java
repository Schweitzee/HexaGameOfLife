package game;

import generated.HexagonalMatrixDrawer;

import javax.swing.*;
import java.awt.*;
import java.util.Set;

public class GamePanel extends JPanel {

    private static final int WIDTH = 2000; // Ablak szélessége
    private static final int HEIGHT = 1000; // Ablak magassága
    HexMatrix table;
    int size = 20;



    public GamePanel(int height, int width){ //, Set<Integer> born, Set<Integer> alive
        table = new HexMatrix(height,width, size);
    }

    public void startGame(){

    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        table.draw(g);
    }
}

