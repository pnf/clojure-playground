package bleh;
import clojure.lang.PersistentHashMap;
// Try to fool the jvm into thinking I'm messing with shared state
public class Noop {
    public static PersistentHashMap store;
    public static void noop(PersistentHashMap o) {
	store = o;
    }
}
