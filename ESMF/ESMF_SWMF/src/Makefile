# by default, this makefile builds the executable 
ALL:  EXE

# Get the SWMF definitions
-include ../../../Makefile.def
-include ../../../Makefile.conf

# Get the dependencies
-include Makefile.DEPEND

# Add ESMF include directories
-include ${ESMFMKFILE}
INCL_EXTRA = ${ESMF_F90COMPILEPATHS}

# The executable
EXE = ${BINDIR}/ESMF_SWMF.exe

# Object files which need to be compiled into the executable.
OBJECTS = \
        SWMF_driver.o \
        SWMF_shared.o \
        IPE_grid_comp.o \
        RIM_grid_comp.o

# Dependencies:
DEPEND:
	perl ${SCRIPTDIR}/depend.pl ${OBJECTS}

EXE: DEPEND
	make ${EXE}
	@echo ' '
	@echo '${EXE} has been brought up to date'
	@echo ' '

# General rules
.F90.o:
	${COMPILE.f90} -c -DsysLinux -Dcompifort ${Cflag2} $<

# Create the library for NUOPC interface
LIB: ${OBJECTS}
	${AR} ${LIBDIR}/libSWMF_nuopc.a ${OBJECTS}

# Build the executable.
$(EXE): LIB 
	@echo 'application:' > esmxBuild.yaml
	@echo '  disable_comps: ESMX_Data' >> esmxBuild.yaml
	@echo '  link_paths: ${LIBDIR}' >> esmxBuild.yaml
	@echo '  link_module_paths: ${INCLDIR}' >> esmxBuild.yaml
	@echo '  link_libraries: SWMF' >> esmxBuild.yaml
	@echo '  exe_name: SWMF_NUOPC.exe' >> esmxBuild.yaml
	@echo 'components:' >> esmxBuild.yaml
	@echo '  swmf:' >> esmxBuild.yaml
	@echo '    build_type: none' >> esmxBuild.yaml
	@echo '    install_prefix: ${DIR}' >> esmxBuild.yaml
	@echo '    include_dir: share/include' >> esmxBuild.yaml
	@echo '    fort_module: swmf_driver.mod' >> esmxBuild.yaml
	@echo '    libraries: libSWMF_nuopc.a' >> esmxBuild.yaml
	${ESMF_APPSDIR}/ESMX_Builder --prefix=${DIR} --verbose

clean: cleanfiles

distclean: clean

