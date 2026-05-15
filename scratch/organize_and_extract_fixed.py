import os
import shutil
import tarfile
import re

def organize_and_extract():
    # Chemin racine — tous les .tar sont directement ici
    base_folder = os.path.expanduser("/home/mazzez/Bureau/R project/Data/0.5° x 0.5°")

    if not os.path.exists(base_folder):
        print(f"Erreur : Le dossier principal '{base_folder}' n'existe pas.")
        return

    # Lister tous les fichiers .tar directement dans base_folder
    all_files = [
        f for f in os.listdir(base_folder)
        if f.endswith('.tar') and not f.startswith('.')
        and os.path.isfile(os.path.join(base_folder, f))
    ]

    print(f"{len(all_files)} fichiers .tar trouvés dans : {base_folder}")

    for file_name in sorted(all_files):
        # --- Extraction de l'année depuis le nom du fichier ---
        # Format 1 : pgbh04.gdas.1979.tar       → année = 1979
        # Format 2 : pgbh.gdas.201101.tar        → année = 2011
        match = re.search(r'\.(\d{4})(\d{2})?\.tar$', file_name)
        if not match:
            print(f"  Ignoré (format inconnu) : {file_name}")
            continue

        year = match.group(1)

        source_file = os.path.join(base_folder, file_name)
        year_dir_path = os.path.join(base_folder, year)

        # Créer le dossier de l'année si nécessaire
        if not os.path.exists(year_dir_path):
            os.makedirs(year_dir_path)
            print(f"  Création du dossier : {year}/")

        dest_file = os.path.join(year_dir_path, file_name)

        try:
            # 1. Déplacer le fichier dans le dossier de l'année
            if source_file != dest_file:
                shutil.move(source_file, dest_file)
                print(f"  Déplacé : {file_name} → {year}/")

            # 2. Extraire l'archive
            print(f"  Extraction de {file_name}...")
            with tarfile.open(dest_file, "r") as tar:
                tar.extractall(path=year_dir_path)
                members = tar.getnames()

            # 3. Vérifier que tous les fichiers extraits sont présents
            all_extracted = True
            missing_files = []
            for member in members:
                member_path = os.path.join(year_dir_path, member)
                if not os.path.exists(member_path):
                    all_extracted = False
                    missing_files.append(member)

            if all_extracted:
                os.remove(dest_file)
                print(f"  ✓ Extraction réussie — archive supprimée : {file_name}")
            else:
                print(f"  ✗ Extraction incomplète. Fichiers manquants : {missing_files}")
                print("    L'archive N'A PAS été supprimée par sécurité.")

        except Exception as e:
            print(f"  ERREUR lors du traitement de {file_name} : {e}")

    print("\nOpération terminée.")

if __name__ == "__main__":
    organize_and_extract()
