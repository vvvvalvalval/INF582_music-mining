package edu.polytechnique.music_mining.tgmodels;

import java.util.List;

import org.herac.tuxguitar.song.models.TGSong;
import org.herac.tuxguitar.song.models.TGTrack;

public class Song extends TGSong {

	public Song() {
		super();
	}

	private List<TGTrack> tracks(){
		return Util.listFromIterator(getTracks());
	}
	
	@Override
	public String toString() {
		return "Song [name=" + getName() + ",\n artist=" + getArtist()
				+ ",\n tracks=" + tracks() + "]";
	}

	
}
