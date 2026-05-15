import os

def list_files_in_folder():
    # Définition des chemins
    # Le tilde (~) est étendu automatiquement pour pointer vers le dossier de l'utilisateur
    source_folder = os.path.expanduser("/home/mazzez/Bureau/R project/Data/0.5° x 0.5°")
    output_file = os.path.expanduser("/home/mazzez/Bureau/R project/Final Version")

    # Vérification si le dossier source existe
    if not os.path.exists(source_folder):
        print(f"Erreur : Le dossier '{source_folder}' n'existe pas.")
        return

    try:
        with open(output_file, 'w', encoding='utf-8') as f_out:
            # Parcours récursif du dossier
            for root, dirs, files in os.walk(source_folder):
                for file in files:
                    # Construction du chemin complet
                    full_path = os.path.join(root, file)
                    # Calcul du chemin relatif pour un affichage propre
                    relative_path = os.path.relpath(full_path, source_folder)
                    
                    # Écriture dans le fichier
                    f_out.write(f"{relative_path}\n")
        
        print(f"Succès ! La liste des fichiers a été générée dans : {output_file}")

    except Exception as e:
        print(f"Une erreur est survenue : {e}")

if __name__ == "__main__":
    list_files_in_folder()
