package game;

import java.util.Set;
import java.util.TreeSet;

public class BasicUtil {
    private BasicUtil() {}

    /**
     * This method is used to make a Set of Integers from a String
     * @param str String to be converted
     * @return Set of Integers
     */
    public static Set<Integer> makeIntSet(String str) {
        TreeSet<Integer> intSet = new TreeSet<>();
        char[] charSet = str.toCharArray();
        for (char c : charSet) {
            intSet.add(Integer.parseInt(Character.toString(c)));
        }
        return intSet;
    }
}
