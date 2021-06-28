# Unsupervised Learning on Spotify Data

### Project for Data Analysis Course of MSc in Data Science for Management (UniCT).

As part of this work, unsupervised learning techniques - such as *Principal Component Analysis* and *Cluster Analysis* - were performed in order to investigate and discover unknown patterns in the data taken under study.

The dataset being analyzed was obtained through the Spotify Web API using the "*spotifyr*" R package, which allows to easily retrieve artists, songs and playlists information, along with audio features. The dataset contains 200 songs picked out from four Spotify playlists - **Rock This**, **Dance Hits**, **Beats & Rhymes**, **Deep Focus** - of different music genre, with their audio features, that is:

* **acousticness**: A confidence measure from 0.0 to 1.0 of whether the track is acoustic. 1.0 represents high confidence the track is acoustic.
* **danceability**: Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable.
* **energy**: Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy.
* **loudness**: The overall loudness of a track in decibels (dB). Loudness values are averaged across the entire track and are useful for comparing relative loudness of tracks. Loudness is the quality of a sound that is the primary psychological correlate of physical strength (amplitude). Values typical range between -60 and 0 db.
* **tempo**: The overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration.
* **valence**: A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry).

In particular, such data analysis on songs audio features involved the application of:

* Principal Component Analysis
* K-means Clustering
* K-medoids Clustering
* Hierarchical Clustering
* Model-based Clustering
