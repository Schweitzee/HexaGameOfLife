package game;

import javax.swing.*;
import java.awt.*;
import java.util.Set;

public class GamePanel extends JPanel {
    HexMatrix table;



    public GamePanel(int height, int width, int hexSize, Set<Integer> born, Set<Integer> alive){ //, Set<Integer> born, Set<Integer> alive
        table = new HexMatrix(height,width, hexSize, born, alive);
    }
    public GamePanel(HexMatrix matrix){
        table = matrix;
    }




    public void startGame(){

    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        super.setBackground(Color.BLACK);
        table.paintComponent(g);
    }
}

