package test;

import game.Hex;

import static org.junit.jupiter.api.Assertions.*;

class HexTest {
    Hex h;

    @org.junit.jupiter.api.BeforeEach
    void setUp() {
        h = new Hex();
    }

    @org.junit.jupiter.api.Test
    void testHex() {
        h.setNewState(true);
        h.switchStateToNewState();
        h.setNewState(false);

        assertTrue(h.getState());       // test setNewState()

        h.switchStateToNewState();
        assertFalse(h.getState());      // test switchStateToNewState()
    }

    @org.junit.jupiter.api.Test
    void testHex2() {
        int size = 5;
        h.initialSet(20,20,size);      // test initialSet()
        assertTrue(h.contains(20,20));
        assertFalse(h.contains(100,100));

        assertTrue(h.contains(20-size+1,20));
        assertTrue(h.contains(20+size-1,20));

        assertTrue(h.contains(20-(int)(0.5*size)+1,20));
        assertTrue(h.contains(20+(int)(0.5*size)-1,20));

        assertTrue(h.contains(20,20- (int)(size*0.5*1.75)+1));
        assertTrue(h.contains(20,20+ (int)(size*0.5*1.75)-1));

        assertFalse(h.contains(20 - size,20- (int)(size*0.5*1.75)+1));      // test contains()
    }
}