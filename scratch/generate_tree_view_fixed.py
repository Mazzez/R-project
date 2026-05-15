import os

def generate_tree_view():
    entrer = input("Veuillez entrer le chemin du fichier source : ").strip()
    numero = input("Veuillez entrer le numéro du fichier : ").strip()

    input_file = os.path.expanduser(entrer)
    output_dir = os.path.expanduser("/home/mazzez/Bureau/R project/Data/Final Version")
    output_file = os.path.join(output_dir, f"tree_view_report_{numero}.txt")

    # Vérification du fichier source
    if not os.path.exists(input_file):
        print(f"Erreur : Le fichier d'entrée '{input_file}' n'existe pas.")
        print("Veuillez d'abord exécuter generate_final_report.py.")
        return

    # --- FIX Bug 1 : Créer le dossier de sortie s'il n'existe pas ---
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
        print(f"Dossier de sortie créé : {output_dir}")

    # Lecture et filtrage des lignes utiles
    with open(input_file, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    path_lines = [
        line.strip() for line in lines
        if line.strip()
        and not line.startswith("Contenu du dossier")
        and not line.startswith("=")
    ]

    if not path_lines:
        print("Erreur : Aucune ligne de chemin trouvée dans le fichier source.")
        return

    # --- FIX Bug 2 : Utiliser '/' comme séparateur (indépendant de l'OS) ---
    # Les chemins dans Final_project utilisent '/' comme séparateur
    tree = {}
    for path in path_lines:
        parts = path.replace("\\", "/").split("/")  # Normalisation
        parts = [p for p in parts if p]  # Supprimer les parties vides
        current_level = tree
        for part in parts:
            if part not in current_level:
                current_level[part] = {}
            current_level = current_level[part]

    # Fonction récursive d'affichage arborescent
    def print_tree(node, prefix=""):
        lines_output = []
        keys = sorted(node.keys())
        for key in keys:
            lines_output.append(f"{prefix}|_{key}")
            if node[key]:  # S'il a des enfants
                lines_output.extend(print_tree(node[key], prefix + "    "))
        return lines_output

    output_lines = print_tree(tree)

    try:
        with open(output_file, 'w', encoding='utf-8') as f_out:
            f_out.write("Rapport Structure Arborescente\n")
            f_out.write("==============================\n\n")
            for line in output_lines:
                f_out.write(line + "\n")

        print(f"Succès ! La vue arborescente a été générée dans : {output_file}")
        print(f"Total éléments affichés : {len(output_lines)}")

    except Exception as e:
        print(f"Une erreur est survenue lors de l'écriture : {e}")

if __name__ == "__main__":
    generate_tree_view()
