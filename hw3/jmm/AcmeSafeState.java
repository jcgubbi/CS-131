import java.util.concurrent.atomic.AtomicLongArray;

class AcmeSafeState implements State {
    private AtomicLongArray value;

    AcmeSafeState(int length) { value = new AtomicLongArray(length); }

    public int size() { return value.length(); }

    public long[] current() {
	int valSize = value.length(); 
	long[] rval = new long[valSize];
	for(int i = 0; i < valSize; i++){
		rval[i] = value.get(i);
	}
	return rval; 
    }

    public void swap(int i, int j) {
	value.getAndDecrement(i);
	value.getAndIncrement(j);
    }
}
