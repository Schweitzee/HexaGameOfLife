package game;

import java.awt.*;

public class Hex {
    private Boolean state = false;
    private Boolean newState = false;
    private final int[] xList = new int[6];
    private final int[] yList = new int[6];

    public Hex(){}

    public void initialSet(int x, int y, int size){

        xList[0] = x - (int)(size*0.5);
        xList[1] = x + (int)(size*0.5);
        xList[2] = x + size;
        xList[3] = x + (int)(size*0.5);
        xList [4] = x - (int)(size*0.5);
        xList[5] = x - size;

        yList[0] = (int) (y - (1.75 * size * 0.5));
        yList[1] = (int) (y - (1.75 * size * 0.5));
        yList[2] = y;
        yList[3] = (int) (y + (1.75 * size * 0.5));
        yList[4] = (int) (y + (1.75 * size * 0.5));
        yList[5] = y;
    }

    //  public int[] getX_s(){return xList;}
    //  public int[] getyList() {return yList;}

    public void draw(Graphics g) {
        Polygon hexagon = new Polygon();
        hexagon.npoints = 6;
        hexagon.xpoints = xList;
        hexagon.ypoints = yList;

        Graphics2D g2d = (Graphics2D) g;
        if (Boolean.TRUE.equals(this.state)){
            g2d.setColor(Color.WHITE);
        }else {
            g2d.setColor(Color.PINK);
        }
        g2d.fill(hexagon);
        g2d.setColor(Color.WHITE);
        g2d.draw(hexagon);
    }

    public void update() { state = newState; }
    public Boolean getState(){
        return state;
    }
    public void setNewState(Boolean newSt) {
        newState = newSt;
    }

    public Boolean contains(int x, int y) {
        Polygon hexagon = new Polygon();
        hexagon.npoints = 6;
        hexagon.xpoints = xList;
        hexagon.ypoints = yList;
        return hexagon.contains(x,y);

    }
}