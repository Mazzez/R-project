import os

def generate_final_report():
    # Définition des chemins
    source_folder = os.path.expanduser("/home/mazzez/Bureau/R project/Data/0.5° x 0.5°")
    output_file = os.path.expanduser("/home/mazzez/Bureau/R project/Final project")

    # Vérification si le dossier source existe
    if not os.path.exists(source_folder):
        print(f"Erreur : Le dossier '{source_folder}' n'existe pas.")
        return

    print(f"Analyse du dossier : {source_folder}...")

    try:
        with open(output_file, 'w', encoding='utf-8') as f_out:
            f_out.write(f"Contenu du dossier : {source_folder}\n")
            f_out.write("=" * 50 + "\n\n")

            # Parcours récursif du dossier
            file_count = 0
            for root, dirs, files in os.walk(source_folder):
                # Trier pour un affichage ordonné
                dirs.sort()
                files.sort()
                
                for file in files:
                    full_path = os.path.join(root, file)
                    relative_path = os.path.relpath(full_path, source_folder)
                    
                    # Ignorer les fichiers cachés si besoin (.DS_Store etc)
                    if not file.startswith('.'):
                        f_out.write(f"{relative_path}\n")
                        file_count += 1
        
        print(f"Succès ! Le rapport a été généré dans : {output_file}")
        print(f"Total fichiers listés : {file_count}")

    except Exception as e:
        print(f"Une erreur est survenue : {e}")

if __name__ == "__main__":
    generate_final_report()
