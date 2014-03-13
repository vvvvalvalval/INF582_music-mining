package edu.polytechnique.music_mining.tgmodels;

import org.herac.tuxguitar.song.factory.TGFactory;
import org.herac.tuxguitar.song.models.TGNote;

/**
 * A fretted note, i.e a fret number and a string.
 * @author val
 *
 */
public class Note extends TGNote {

	public Note(TGFactory factory) {
		super(factory);
	}

	@Override
	public String toString() {
		return "Note [value=" + getValue() + ", string="
				+ getString() + "]";
	}

	
}
