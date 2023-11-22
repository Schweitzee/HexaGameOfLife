package test;

import game.BasicUtil;
import game.Hex;
import game.HexMatrix;

import static org.junit.jupiter.api.Assertions.*;

public class HexMatrixTest {
    HexMatrix hm;

    Hex[][] hexes;

   @org.junit.jupiter.api.BeforeEach
   void setUp() {
       hm = new HexMatrix(10,10,20, BasicUtil.makeIntSet("3"),BasicUtil.makeIntSet("23"));
       hm.getHexes()[0][0].setNewState(true);
       hm.getHexes()[0][0].switchStateToNewState();

       hexes = hm.getHexes();
   }

    @org.junit.jupiter.api.Test
    void getWi() {
        assert hm.getWi() == 10;
    }

    @org.junit.jupiter.api.Test
    void getHe() {
        assert hm.getHe() == 10;
    }

    @org.junit.jupiter.api.Test
    void getHexSize() {
        assert hm.getHexSize() == 20;
    }

    @org.junit.jupiter.api.Test
    void getHexes() {
        assert hm.getHexes().length == 10;
        assert hm.getHexes()[0].length == 10;

        assertSame(hm.getHexes(), hexes);
    }

    @org.junit.jupiter.api.Test
    void getSurroundingsTest(){
        for (Hex[] row: hm.getHexes()){
            for(Hex hex: row){
                hex.setNewState(true);
                hex.switchStateToNewState();
            }
        }

       assertEquals(6, hm.getSurroundings(2,2));
    }

    @org.junit.jupiter.api.Test
    void getBornSetTest(){
       assertEquals(3, hm.getBornSet());
    }

    @org.junit.jupiter.api.Test
    void getAliveSetTest(){
       assertEquals(23, hm.getAliveSet());
    }

}
