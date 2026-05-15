import os
import shutil
import tarfile

def organize_and_extract():
    # Chemin racine
    base_folder = os.path.expanduser("/home/mazzez/Bureau/R project/Data/0.5° x 0.5°")

    if not os.path.exists(base_folder):
        print(f"Erreur : Le dossier principal '{base_folder}' n'existe pas.")
        return

    # Identifier les sous-dossiers dans 'R project'
    # On ne prend que les dossiers, pas les fichiers à la racine de 'R project'
    subfolders = [f for f in os.listdir(base_folder) if os.path.isdir(os.path.join(base_folder, f))]

    print(f"Dossiers trouvés à traiter : {subfolders}")

    for folder_name in subfolders:
        current_folder_path = os.path.join(base_folder, folder_name)
        print(f"\nTraitement du dossier : {folder_name}")

        # Liste des fichiers dans ce sous-dossier
        files = os.listdir(current_folder_path)

        for year in range(1979, 2027):
            str_year = str(year)
            
            # Trouver les fichiers .tar qui contiennent l'année dans leur nom
            # On ignore les fichiers qui commencent par '.' (cachés/temporaires)
            year_files = [f for f in files if f.endswith('.tar') and str_year in f and not f.startswith('.')]

            if year_files:
                # Créer le dossier de l'année
                year_dir_path = os.path.join(current_folder_path, str_year)
                if not os.path.exists(year_dir_path):
                    os.makedirs(year_dir_path)
                    print(f"  Création du dossier : {year_dir_path}")

                for file_name in year_files:
                    source_file = os.path.join(current_folder_path, file_name)
                    dest_file = os.path.join(year_dir_path, file_name)

                    try:
                        # 1. Déplacer le fichier
                        # Vérifier si on ne déplace pas vers soi-même (si déjà fait)
                        if source_file != dest_file:
                             shutil.move(source_file, dest_file)
                             print(f"    Déplacé : {file_name} -> {str_year}/")
                        
                        # 2. Extraire le fichier
                        # On extrait dans le dossier de l'année
                        # 2. Extraire le fichier
                        print(f"    Extraction de {file_name}...")
                        with tarfile.open(dest_file, "r") as tar:
                            tar.extractall(path=year_dir_path)
                            members = tar.getnames()
                        
                        print("      Extraction terminée. Vérification des fichiers...")

                        # Vérification rigoureuse : est-ce que tous les fichiers sont là ?
                        all_extracted = True
                        missing_files = []
                        for member in members:
                            member_path = os.path.join(year_dir_path, member)
                            if not os.path.exists(member_path):
                                all_extracted = False
                                missing_files.append(member)
                        
                        if all_extracted:
                            print("      Vérification réussie : Tous les fichiers sont présents.")
                            # 3. Supprimer l'archive pour gagner de l'espace
                            os.remove(dest_file)
                            print(f"      Archive supprimée pour libérer de l'espace : {file_name}")
                        else:
                            print(f"      ATTENTION : Extraction incomplète. Fichiers manquants : {missing_files}")
                            print("      L'archive N'A PAS été supprimée par sécurité.")

                    except Exception as e:
                        print(f"    ERREUR lors du traitement de {file_name} : {e}")

    print("\nOpération terminée.")

if __name__ == "__main__":
    organize_and_extract()
