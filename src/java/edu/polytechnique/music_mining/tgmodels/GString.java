package edu.polytechnique.music_mining.tgmodels;

import org.herac.tuxguitar.song.models.TGString;

/**
 * A String of a guitar.
 * @author val
 *
 */
public class GString extends TGString {

	public GString() {
	}

	@Override
	public String toString() {
		return "GString [number()=" + getNumber() + ", value()="
				+ getValue() + "]";
	}

	
}
