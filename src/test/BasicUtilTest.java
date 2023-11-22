package test;

import game.BasicUtil;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
public class BasicUtilTest {

    @org.junit.jupiter.api.Test
    public void makeIntSetTest() {
        String str = "123456789";
        Set<Integer> intSet = BasicUtil.makeIntSet(str);
        assertEquals(9, intSet.size());
        for (int i = 1; i < 10; i++) {
            assertTrue(intSet.contains(i));
        }
    }

    @org.junit.jupiter.api.Test
    public void makeIntSetTest2() {
        String str = "13579";

        Set<Integer> intSet = BasicUtil.makeIntSet(str);

        assertEquals(5, intSet.size());

        for (int i = 1; i < 10; i++) {
            if(i%2 == 1){
                assertTrue(intSet.contains(i));
            }
            else{
                assertFalse(intSet.contains(i));
            }
        }
    }
    @org.junit.jupiter.api.Test
    public void makeIntSetTest3() {
        String str = "02468";

        Set<Integer> intSet = BasicUtil.makeIntSet(str);

        assertEquals(5, intSet.size());

        for (int i = 1; i < 10; i++) {
            if(i%2 == 0){
                assertTrue(intSet.contains(i));
            }
            else{
                assertFalse(intSet.contains(i));
            }
        }
    }
}
