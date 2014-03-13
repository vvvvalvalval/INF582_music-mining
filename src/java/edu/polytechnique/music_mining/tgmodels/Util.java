package edu.polytechnique.music_mining.tgmodels;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public final class Util {

	/**
	 * Creates a list filled with the values from an {@link Iterator}.
	 * @param iterator
	 * @return
	 */
	public static <T> List<T> listFromIterator(Iterator<? extends T> iterator){
		List<T> res = new ArrayList<T>();
		while(iterator.hasNext()){
			res.add(iterator.next());
		}
		return res;
	}
}
