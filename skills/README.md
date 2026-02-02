# Skills Setup para Asistentes de IA

Este directorio contiene los scripts necesarios para configurar los skills de IA en diferentes asistentes de código (Claude, Gemini, Codex, GitHub Copilot).

## 📁 Archivos

- **`setup.sh`**: Script principal de configuración
- **`skills/*/SKILL.md`**: Archivos de documentación de cada skill

## 🚀 Uso Rápido

### Configuración Inicial

```bash
# Modo interactivo (recomendado para primera vez)
./skills/setup.sh

# Configurar todos los asistentes
./skills/setup.sh --all

# Configurar solo Claude
./skills/setup.sh --claude

# Configurar múltiples asistentes
./skills/setup.sh --claude --copilot --gemini

# Con powershell
wsl bash skills/setup.sh
```

### Opciones Disponibles

| Opción | Descripción |
|--------|-------------|
| `--all` | Configura todos los asistentes de IA |
| `--claude` | Configura Claude Code |
| `--gemini` | Configura Gemini CLI |
| `--codex` | Configura Codex (OpenAI) |
| `--copilot` | Configura GitHub Copilot |
| `--help` | Muestra la ayuda |

## 🔧 Qué hace `setup.sh`

### 1. Crea Enlaces Simbólicos (Symlinks)

Crea enlaces simbólicos que apuntan a la carpeta `skills/`:

```
.claude/skills  → skills/
.gemini/skills  → skills/
.codex/skills   → skills/
```

**Ventaja:** Cualquier skill nuevo que agregues en `skills/` está disponible automáticamente.

### 2. Copia el Archivo AGENTS.md

Según el asistente seleccionado:

| Asistente | Archivo Generado | Ubicación |
|-----------|------------------|-----------|
| Claude Code | `CLAUDE.md` | Todos los directorios con `AGENTS.md` |
| Gemini CLI | `GEMINI.md` | Todos los directorios con `AGENTS.md` |
| GitHub Copilot | `copilot-instructions.md` | `.github/` |
| Codex (OpenAI) | *Usa `AGENTS.md` nativo* | No copia archivos |

### 3. Cuenta y Valida Skills

- Busca todos los archivos `SKILL.md` en subdirectorios
- Valida que existan skills antes de continuar
- Muestra el total de skills encontrados

## ➕ Agregar un Nuevo Skill

### Paso 1: Crear la Estructura

```bash
mkdir -p skills/mi-nuevo-skill
```

### Paso 2: Crear el SKILL.md

```bash
cat > skills/mi-nuevo-skill/SKILL.md << 'EOF'
# Mi Nuevo Skill

Descripción breve del skill (máximo 10 palabras).

## Contenido

[Aquí va la documentación del skill]
EOF
```

### Paso 3: Actualizar AGENTS.md (Opcional)

Si quieres que aparezca en el índice principal, agrega una entrada en `AGENTS.md`:

```markdown
| Mi Nuevo Skill | Descripción breve | [mi-nuevo-skill](./skills/mi-nuevo-skill/SKILL.md) |
```

### Paso 4: Re-ejecutar Setup (Solo si modificaste AGENTS.md)

```bash
./skills/setup.sh --all
```

> **Nota:** Si solo agregaste el skill sin modificar `AGENTS.md`, el skill ya estará disponible automáticamente gracias a los symlinks.

## 🔄 Actualización de Skills

### Escenarios:

| Escenario | ¿Necesitas re-ejecutar setup.sh? |
|-----------|----------------------------------|
| Agregar nuevo skill en `skills/nueva-skill/` | ❌ No (symlink lo detecta automáticamente) |
| Modificar contenido de un `SKILL.md` existente | ❌ No (se lee en tiempo real) |
| Modificar `AGENTS.md` (índice principal) | ✅ Sí (para actualizar copias) |
| Cambiar de asistente de IA | ✅ Sí (ejecutar con nuevo flag) |

## 📋 Estructura de Archivos Generada

Después de ejecutar `setup.sh --all`, tendrás:

```
.
├── .claude/
│   └── skills → ../skills/          (symlink)
├── .codex/
│   └── skills → ../skills/          (symlink)
├── .gemini/
│   └── skills → ../skills/          (symlink)
├── .github/
│   └── copilot-instructions.md      (copia de AGENTS.md)
├── skills/
│   ├── setup.sh
│   ├── setup_test.sh
│   ├── about-intersystems/
│   │   └── SKILL.md
│   ├── about-tls/
│   │   └── SKILL.md
│   └── ...
├── AGENTS.md                         (fuente original)
├── CLAUDE.md                         (copia para Claude)
└── GEMINI.md                         (copia para Gemini)
```

## 🐛 Troubleshooting

### Error: "No skills found"

```bash
# Verifica que existan archivos SKILL.md
find skills/ -name "SKILL.md"
```

### Los cambios no se reflejan en el asistente

1. **Reinicia tu asistente de IA**
2. Si modificaste `AGENTS.md`, re-ejecuta:
   ```bash
   ./skills/setup.sh --all
   ```

### Permisos en Linux/WSL

```bash
chmod +x skills/setup.sh
```

### Backup de configuración anterior

El script automáticamente hace backup si detecta configuraciones existentes:

```
.claude/skills.backup.1738598400
```

## 📝 Notas Importantes

- **AGENTS.md es la fuente de verdad**: Edita este archivo y luego re-ejecuta `setup.sh`
- **Los symlinks son dinámicos**: Nuevos skills se detectan automáticamente
- **Idempotente**: Puedes ejecutar `setup.sh` múltiples veces sin problemas
- **Reinicia tu asistente**: Después de configurar, reinicia para cargar los skills

## 🤝 Workflow Recomendado

1. **Primera configuración:**
   ```bash
   ./skills/setup.sh --all
   ```

2. **Agregar nuevo skill:**
   ```bash
   mkdir -p skills/nuevo-skill
   # Crear SKILL.md
   # Opcionalmente actualizar AGENTS.md
   ./skills/setup.sh --all  # Solo si editaste AGENTS.md
   ```

3. **Antes de hacer cambios importantes:**
   ```bash
   ./skills/setup_test.sh  # Validar que todo funcione
   ```

4. **Reiniciar asistente de IA** para cargar los cambios

---

**Desarrollado siguiendo el estándar [agentskills.io](https://agentskills.io)**
