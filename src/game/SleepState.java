package game;

/**
 * SleepStates are used to determine how fast the game should run.
 */
public enum SleepState{

    /**
     * Normal speed represents 500 milliseconds between updates
     */
    NORMAL,
    /**
     * Fast speed represents 350 milliseconds between updates
     */
    FAST,
    /**
     * Faster speed represents 200 milliseconds between updates
     */
    FASTER,
    /**
     * Fastest speed represents 100 milliseconds between updates
     */
    FASTEST,
    /**
     * Slow speed represents 750 milliseconds between updates
     */
    SLOW,
    /**
     * Slowest speed represents 1000 milliseconds between updates
     */
    SLOWEST
}