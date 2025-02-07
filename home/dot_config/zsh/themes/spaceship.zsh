# Spaceship-prompt

SPACESHIP_PROMPT_SEPARATE_LINE=false     # Make the prompt span across two lines
SPACESHIP_PROMPT_FIRST_PREFIX_SHOW=true  # Shows a prefix of the first section in prompt
SPACESHIP_PROMPT_PREFIXES_SHOW=true      # Show prefixes before prompt sections or not

# SPACESHIP_CHAR_SYMBOL="▶ "
# SPACESHIP_CHAR_SYMBOL=" "
# SPACESHIP_CHAR_SYMBOL=" "
# SPACESHIP_CHAR_SYMBOL="$ "
# SPACESHIP_CHAR_SYMBOL=" "
SPACESHIP_CHAR_SYMBOL=" "

SPACESHIP_DIR_PREFIX=' '               # Prefix before current directory
# SPACESHIP_DIR_TRUNC=3                  # Number of folders of cwd to show in prompt, 0 to show all
# SPACESHIP_DIR_TRUNC_PREFIX='…/'        # Prefix before cwd when it's truncated. For example …/ or .../, empty to disable

SPACESHIP_GIT_SYMBOL=' '
# SPACESHIP_GIT_SYMBOL=' '
# SPACESHIP_GIT_SYMBOL=' '
# SPACESHIP_GIT_SYMBOL='שׂ '

# SPACESHIP_GIT_PREFIX=''               # Prefix before Git section

# SPACESHIP_GIT_STATUS_DELETED=''  # bold
# SPACESHIP_GIT_STATUS_DELETED=''  # semi
# SPACESHIP_GIT_STATUS_DELETED='窱'  # semi
# SPACESHIP_GIT_STATUS_DELETED=''  # light

SPACESHIP_CONDA_SHOW=true
# SPACESHIP_CONDA_SYMBOL=' '
# SPACESHIP_CONDA_SYMBOL=' '
# SPACESHIP_CONDA_SYMBOL=' '
# SPACESHIP_CONDA_SYMBOL=' '
# SPACESHIP_CONDA_SYMBOL=' '
# SPACESHIP_CONDA_SYMBOL='⏾ '
# SPACESHIP_CONDA_SYMBOL='ﭰ '
# SPACESHIP_CONDA_SYMBOL=' '
# SPACESHIP_CONDA_SYMBOL=' '
# SPACESHIP_CONDA_SYMBOL=' '
SPACESHIP_CONDA_SYMBOL=' '

SPACESHIP_PROMPT_ORDER=(
    # time          # Time stamps section
    user          # Username section
    dir           # Current directory section
    host          # Hostname section
    git           # Git section (git_branch + git_status)
    hg            # Mercurial section (hg_branch  + hg_status)
    package       # Package version
    node          # Node.js section
    ruby          # Ruby section
    elixir        # Elixir section
    xcode         # Xcode section
    swift         # Swift section
    golang        # Go section
    php           # PHP section
    rust          # Rust section
    haskell       # Haskell Stack section
    julia         # Julia section
    docker        # Docker section
    aws           # Amazon Web Services section
    venv          # virtualenv section
    conda         # conda virtualenv section
    pyenv         # Pyenv section
    dotnet        # .NET section
    ember         # Ember.js section
    # kubecontext   # Kubectl context section
    terraform     # Terraform workspace section
    # exec_time     # Execution time
    line_sep      # Line break
    # battery       # Battery level and status
    vi_mode       # Vi-mode indicator
    jobs          # Background jobs indicator
    exit_code     # Exit code section
    char          # Prompt character
)

SPACESHIP_RPROMPT_ORDER=(
    exec_time     # Execution time
)

