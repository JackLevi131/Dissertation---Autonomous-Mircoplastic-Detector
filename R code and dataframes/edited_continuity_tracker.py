import pandas as pd
import numpy as np
from pathlib import Path


min_track_length = 19  
# settings for coordinate continuity
max_frame_gap = 18     
dist_multipy = 1.5  

start = {'x': 100, 'y': 900, 'radius': 100}  
end = {'x': 260, 'y': 160, 'radius': 100}   


FRAME_COL = "frame"
ID_COL = "id"
X_COL = "x"
Y_COL = "y"

def detect_model_type(df):
    static_cols = ['primary_static_class', 'secondary_static_class']
    motion_cols = ['primary_motion_class', 'secondary_motion_class']
    
    has_static = all(col in df.columns for col in static_cols) and df[static_cols].notna().any().any()
    has_motion = all(col in df.columns for col in motion_cols) and df[motion_cols].notna().any().any()
    
    if has_static and not has_motion:
        return 'static'
    elif has_motion and not has_static:
        return 'motion'
    elif has_static and has_motion:
        return 'both'
    return 'unknown'

# claculate average movement for a object per frame 
def calculate_avg_movement(track_df):
    sorted_track = track_df.sort_values(FRAME_COL)
    total_distance = 0
    frame_pairs = 0
    
    for i in range(1, len(sorted_track)):
        prev_row = sorted_track.iloc[i-1]
        curr_row = sorted_track.iloc[i]
        
        frame_diff = curr_row[FRAME_COL] - prev_row[FRAME_COL]
        
        # counts ~consecutive frames
        if frame_diff <= 3:
            distance = np.sqrt(
                (curr_row[X_COL] - prev_row[X_COL])**2 + 
                (curr_row[Y_COL] - prev_row[Y_COL])**2
            )
            total_distance += distance
            frame_pairs += 1
    
    return total_distance / frame_pairs if frame_pairs > 0 else 0

# need to merge broken tracks using average movement
def merge_broken_tracks(df):

    movements = []
    for track_id, track_data in df.groupby(ID_COL):
        avg_mov = calculate_avg_movement(track_data)
        if avg_mov > 0:
            movements.append(avg_mov)
    
    avg_pixels_per_frame = np.mean(movements) if movements else 30
    
    tracks = []
    for track_id, track_data in df.groupby(ID_COL):
        sorted_track = track_data.sort_values(FRAME_COL)
        tracks.append({
            'id': track_id,
            'first_frame': sorted_track[FRAME_COL].min(),
            'last_frame': sorted_track[FRAME_COL].max(),
            'first_x': sorted_track.iloc[0][X_COL],
            'first_y': sorted_track.iloc[0][Y_COL],
            'last_x': sorted_track.iloc[-1][X_COL],
            'last_y': sorted_track.iloc[-1][Y_COL],
            'merged_ids': [track_id]
        })
    
    # Sort by first frame
    tracks.sort(key=lambda t: t['first_frame'])
    
    original_count = len(tracks)
    
    # keep merging tracks
    merged_something = True
    merge_rounds = 0
    
    while merged_something:
        merged_something = False
        merge_rounds += 1
        i = 0
        
        while i < len(tracks):
            track_a = tracks[i]
            merged_this_iteration = False
            
            for j in range(i + 1, len(tracks)):
                track_b = tracks[j]
                
                #  frame gap calcultion
                frame_gap = track_b['first_frame'] - track_a['last_frame']
                
                # Skip gap
                if frame_gap <= 0 or frame_gap > max_frame_gap:
                    continue
                
                
                spatial_distance = np.sqrt(
                    (track_b['first_x'] - track_a['last_x'])**2 +
                    (track_b['first_y'] - track_a['last_y'])**2
                )
                
                
                max_distance = avg_pixels_per_frame * frame_gap * dist_multipy
                
               
                if spatial_distance <= max_distance:
                    
                    track_a['last_frame'] = track_b['last_frame']
                    track_a['last_x'] = track_b['last_x']
                    track_a['last_y'] = track_b['last_y']
                    track_a['merged_ids'].extend(track_b['merged_ids'])
                    
                    
                    tracks.pop(j)
                    merged_something = True
                    merged_this_iteration = True
                    break
            
            if not merged_this_iteration:
                i += 1
    

    
 

    id_mapping = {}
    for track in tracks:
        primary_id = track['merged_ids'][0]  # Use first ID as primary
        merged_id_str = ','.join(map(str, track['merged_ids']))
        
        for old_id in track['merged_ids']:
            id_mapping[old_id] = {
                'primary_id': primary_id,
                'merged_ids': merged_id_str,
                'num_segments': len(track['merged_ids'])
            }
    
    return id_mapping, avg_pixels_per_frame







def process_csv(csv_path):
    
    df = pd.read_csv(csv_path)
    model_type = detect_model_type(df)
    print(f"  model type: {model_type}")
    
    # Apply coordinate continuity merging
    id_mapping, avg_movement = merge_broken_tracks(df)
    
    # 
    df['merged_id'] = df[ID_COL].map(lambda x: id_mapping[x]['primary_id'])
    df['merged_ids'] = df[ID_COL].map(lambda x: id_mapping[x]['merged_ids'])
    df['num_segments'] = df[ID_COL].map(lambda x: id_mapping[x]['num_segments'])
    
    # filter by track length 
    track_lengths = df.groupby('merged_id')[FRAME_COL].count()
    print("Track length stats:")
    print(track_lengths.describe())
    #worked 
    valid_ids = track_lengths[track_lengths >= min_track_length].index
    df = df[df['merged_id'].isin(valid_ids)]
    
    
    
   
    objects = []
    
    for merged_id, track in df.groupby('merged_id'):
        duration = len(track)
        votes = {}
        
        
        merged_ids_str = track['merged_ids'].iloc[0]
        num_segments = track['num_segments'].iloc[0]
        
        for _, row in track.iterrows():
          
            if model_type == 'static':
                primary_cls = row.get('primary_static_class')
                primary_conf = row.get('primary_static_conf', 0)
                secondary_cls = row.get('secondary_static_class')
                secondary_conf = row.get('secondary_static_conf', 0)
            elif model_type == 'motion':
                primary_cls = row.get('primary_motion_class')
                primary_conf = row.get('primary_motion_conf', 0)
                secondary_cls = row.get('secondary_motion_class')
                secondary_conf = row.get('secondary_motion_conf', 0)
            else:  
                primary_cls = row.get('primary_static_class') or row.get('primary_motion_class')
                primary_conf = row.get('primary_static_conf', 0) or row.get('primary_motion_conf', 0)
                secondary_cls = row.get('secondary_static_class') or row.get('secondary_motion_class')
                secondary_conf = row.get('secondary_static_conf', 0) or row.get('secondary_motion_conf', 0)
            
            # assed bias corection to try and even out errors
            BEAD_PENALTY = 0.6      
            FRAG_BOOST = 1.4       
            
            if pd.notna(primary_cls) and str(primary_cls).lower() != 'object':
                conf = primary_conf
                
                # Apply bias correction
                if primary_cls == 'Bead':
                    conf *= BEAD_PENALTY
                elif primary_cls in ['Frag', 'Frags']:
                    conf *= FRAG_BOOST
                
                votes[primary_cls] = votes.get(primary_cls, 0) + conf
            
            if pd.notna(secondary_cls) and str(secondary_cls).lower() != 'object':
                conf = secondary_conf
                
                if secondary_cls == 'Bead':
                    conf *= BEAD_PENALTY
                elif secondary_cls in ['Frag', 'Frags']:
                    conf *= FRAG_BOOST
                
                votes[secondary_cls] = votes.get(secondary_cls, 0) + conf
        
        if not votes:
            final_class = "Unknown"
            mean_conf = 0
        else:
            final_class = max(votes, key=votes.get)
            mean_conf = votes[final_class] / duration
        
        objects.append({
            "object_id": merged_id,
            "original_ids": merged_ids_str,
            "num_segments": num_segments,
            "duration_frames": duration,
            "final_class": final_class,
            "mean_confidence": round(mean_conf, 3)
        })
    
    objects_df = pd.DataFrame(objects)
    
    # sumarise
    summary = objects_df["final_class"].value_counts().to_dict()
    summary_row = {
        "total_plastics": len(objects_df),
        "Bead": summary.get("Bead", 0),
        "Frag": summary.get("Frag", 0),
        "Frags": summary.get("Frags", 0),
        "Organic": summary.get("Organic", 0),
        "avg_segments_per_plastic": round(objects_df["num_segments"].mean(), 1)
    }
    summary_df = pd.DataFrame([summary_row])
    
    # 
    output_dir = csv_path.parent
    base_name = csv_path.stem
    
    objects_out = output_dir / f"{base_name}_objects.csv"
    summary_out = output_dir / f"{base_name}_summary.csv"
    
    objects_df.to_csv(objects_out, index=False)
    summary_df.to_csv(summary_out, index=False)
    
    return len(objects_df)


tracking_files = list(Path(".").glob("*_tracking.csv"))
print("\nfinished")
