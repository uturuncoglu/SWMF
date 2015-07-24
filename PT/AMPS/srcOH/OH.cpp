//$Id$

#include "OH.h"

// user defined global time step
double OH::UserGlobalTimeStep = -1.0;

//  injection boundary condition
double OH::InjectionVelocity[3] = {26.3E3, 0.0, -2.3E3};
double OH::InjectionNDensity    = 0.18E-6;
double OH::InjectionTemperature = 6519;

// computational domain size
double OH::DomainXMin[3] = {-2.25E14,-2.25E14,-2.25E14};
double OH::DomainXMax[3] = { 2.25E14, 2.25E14, 2.25E14};
double OH::DomainDXMin   = 1.8E13;
double OH::DomainDXMax   = 1.8E13;



// OUTPUT ---------------------------------------------------------------------
int OH::Output::TotalDataLength = 0; 
int OH::Output::ohSourceDensityOffset =-1; 
int OH::Output::ohSourceMomentumOffset=-1;
int OH::Output::ohSourceEnergyOffset  =-1;


void OH::Output::PrintVariableList(FILE* fout,int DataSetNumber) {
  fprintf(fout,",\"ohSourceDensity\",\"ohSourceMomentumX\",\"ohSourceMomentumY\",\"ohSourceMomentumZ\",\"ohSourceEnergy\"");
}

void OH::Output::Interpolate(PIC::Mesh::cDataCenterNode** InterpolationList,double *InterpolationCoeficients,int nInterpolationCoeficients,PIC::Mesh::cDataCenterNode *CenterNode){

  double S1=0.0, S2[3]={0.0}, S3=0.0;
  int i,idim;
  char *SamplingBuffer;

  for (i=0;i<nInterpolationCoeficients;i++) {

    S1+=(*((double*)(InterpolationList[i]->GetAssociatedDataBufferPointer()+OH::Output::ohSourceDensityOffset)))*InterpolationCoeficients[i];
    
    for(idim=0 ; idim<3; idim++)
      S2[idim]+=(*(idim+(double*)(InterpolationList[i]->GetAssociatedDataBufferPointer()+OH::Output::ohSourceMomentumOffset)))*InterpolationCoeficients[i];

    S3+=(*((double*)(InterpolationList[i]->GetAssociatedDataBufferPointer()+OH::Output::ohSourceEnergyOffset)))*InterpolationCoeficients[i];
  }

  memcpy(CenterNode->GetAssociatedDataBufferPointer()+OH::Output::ohSourceDensityOffset,&S1,sizeof(double));
  memcpy(CenterNode->GetAssociatedDataBufferPointer()+OH::Output::ohSourceMomentumOffset,&S2,3*sizeof(double));
  memcpy(CenterNode->GetAssociatedDataBufferPointer()+OH::Output::ohSourceEnergyOffset,&S3,sizeof(double));

}

void OH::Output::PrintData(FILE* fout,int DataSetNumber,CMPI_channel *pipe,int CenterNodeThread,PIC::Mesh::cDataCenterNode *CenterNode){
  double t;

  //SourceDensity
  if (pipe->ThisThread==CenterNodeThread) {
    t= *((double*)(CenterNode->GetAssociatedDataBufferPointer()+OH::Output::ohSourceDensityOffset));
  }

  if (pipe->ThisThread==0) {
    if (CenterNodeThread!=0) pipe->recv(t,CenterNodeThread);

    fprintf(fout,"%e ",t);
  }
  else pipe->send(t);

  //SourceMomentum
  for(int idim=0; idim < 3; idim++){
    if (pipe->ThisThread==CenterNodeThread) {
      t= *(idim+(double*)(CenterNode->GetAssociatedDataBufferPointer()+OH::Output::ohSourceMomentumOffset));
    }
    
    if (pipe->ThisThread==0) {
      if (CenterNodeThread!=0) pipe->recv(t,CenterNodeThread);
      
      fprintf(fout,"%e ",t);
    }
    else pipe->send(t);
  }

  //SourceEnergy
  if (pipe->ThisThread==CenterNodeThread) {
    t= *((double*)(CenterNode->GetAssociatedDataBufferPointer()+OH::Output::ohSourceEnergyOffset));
  }

  if (pipe->ThisThread==0) {
    if (CenterNodeThread!=0) pipe->recv(t,CenterNodeThread);

    fprintf(fout,"%e ",t);
  }
  else pipe->send(t);

}

int OH::Output::RequestDataBuffer(int offset){
  OH::Output::ohSourceDensityOffset=offset;
  OH::Output::TotalDataLength = 1;
  offset+=sizeof(double);

  OH::Output::ohSourceMomentumOffset=offset;
  OH::Output::TotalDataLength+=3;
  offset+=3*sizeof(double);

  OH::Output::ohSourceEnergyOffset=offset;
  OH::Output::TotalDataLength++;
  offset+=sizeof(double);

  return OH::Output::TotalDataLength*sizeof(double);
}


void OH::Output::Init() {
  //request sampling buffer and particle fields
  PIC::IndividualModelSampling::RequestStaticCellData.push_back(OH::Output::RequestDataBuffer);

  //print out of the otuput file
  PIC::Mesh::PrintVariableListCenterNode.push_back(OH::Output::PrintVariableList);
  PIC::Mesh::PrintDataCenterNode.push_back(OH::Output::PrintData);
  PIC::Mesh::InterpolateCenterNode.push_back(OH::Output::Interpolate);
}


// Loss -------------------------------------------------------------------------------------

double OH::Loss::LifeTime(double *x, int spec, long int ptr,bool &PhotolyticReactionAllowedFlag,cTreeNodeAMR<PIC::Mesh::cDataBlockAMR> *node){

  double PlasmaNumberDensity, PlasmaPressure, PlasmaTemperature;
  double PlasmaBulkVelocity[3];

  double lifetime=0.0;

  PIC::CPLR::InitInterpolationStencil(x,node);

  PlasmaNumberDensity = PIC::CPLR::GetBackgroundPlasmaNumberDensity();
  PlasmaPressure      = PIC::CPLR::GetBackgroundPlasmaPressure();
  PlasmaTemperature   = PlasmaPressure / (Kbol * PlasmaNumberDensity);
  PIC::CPLR::GetBackgroundPlasmaVelocity(PlasmaBulkVelocity);

  PhotolyticReactionAllowedFlag=true;

  // the model has hydrogen only
  if (spec!=_H_SPEC_) {
    PhotolyticReactionAllowedFlag=false;
    return -1.0;
  }

  // velocity of a particle
  double v[3];
  PIC::ParticleBuffer::GetV(v,ptr);  
  //-------------------
  switch (spec) {
  case _H_SPEC_:
    lifetime= ChargeExchange::LifeTime(_H_SPEC_, v, PlasmaBulkVelocity, PlasmaTemperature, PlasmaNumberDensity);
    break;
  default:
    exit(__LINE__,__FILE__,"Error: unknown specie");
  }

  return lifetime;

}

int OH::Loss::ReactionProcessor(double *xInit,double *xFinal,double *vFinal,long int ptr,int &spec,PIC::ParticleBuffer::byte *ParticleData, cTreeNodeAMR<PIC::Mesh::cDataBlockAMR>* node){
  //for one lost particle one new particle is generated
  //----------------------------------------------------------------------
  //inject the products of the reaction
  double ParentTimeStep,ParentParticleWeight;
  
#if  _SIMULATION_PARTICLE_WEIGHT_MODE_ == _SPECIES_DEPENDENT_GLOBAL_PARTICLE_WEIGHT_
  ParentParticleWeight=PIC::ParticleWeightTimeStep::GlobalParticleWeight[spec];
#else
  ParentParticleWeight=0.0;
  exit(__LINE__,__FILE__,"Error: the weight mode is node defined");
#endif

#if _SIMULATION_TIME_STEP_MODE_ == _SPECIES_DEPENDENT_GLOBAL_TIME_STEP_
  ParentTimeStep=PIC::ParticleWeightTimeStep::GlobalTimeStep[spec];
#else
  ParentTimeStep=0.0;
  exit(__LINE__,__FILE__,"Error: the time step node is not defined");
#endif


  //account for the parent particle correction factor
  ParentParticleWeight*=PIC::ParticleBuffer::GetIndividualStatWeightCorrection(ParticleData);

  //the particle buffer used to set-up the new particle data
  char tempParticleData[PIC::ParticleBuffer::ParticleDataLength];
  PIC::ParticleBuffer::SetParticleAllocated((PIC::ParticleBuffer::byte*)tempParticleData);

  //copy the state of the initial parent particle into the new-daugher particle
  PIC::ParticleBuffer::CloneParticle((PIC::ParticleBuffer::byte*)tempParticleData,ParticleData);

  // injection time of the particle (after the beginning of AMPS' time step)
  double ModelParticleInjectionRate,TimeCounter=0.0,TimeIncrement,ProductWeightCorrection=1.0;
  ModelParticleInjectionRate=1.0/ParentTimeStep;
  TimeIncrement=-log(rnd())/ModelParticleInjectionRate * rnd(); 
  TimeCounter += TimeIncrement;
  
  //generate a particle
  // new particle comes from solar wind and has velocity ~ plasma bulk velocity
  double PlasmaBulkVelocity[3];
  {
    PIC::CPLR::InitInterpolationStencil(xFinal,node);
    PIC::CPLR::GetBackgroundPlasmaVelocity(PlasmaBulkVelocity);
    
    // charge exchange process transfers momentum and energy to plasma
    PIC::Mesh::cDataCenterNode *CenterNode;
    char *offset;
    CenterNode=node->block->GetCenterNode(nd);
    offset=CenterNode->GetAssociatedDataBufferPointer()+PIC::Mesh::collectingCellSampleDataPointerOffset; 
    double v2 = 0.0, plasmav2 = 0.0;
    double c = ParentParticleWeight 
                   / PIC::ParticleWeightTimeStep::GlobalTimeStep[spec]
                   / CenterNode->Measure;
    *((double*)(offset+OH::Output::ohSourceDensityOffset)) += 0.0; 
    for(int idim=0; idim<3; idim++){
      *(idim + (double*)(offset+OH::Output::ohSourceMomentumOffset)) += 
	c*_MASS_(_H_)*(vFinal[idim]-PlasmaBulkVelocity[idim]); 
      v2      +=vFinal[idim]*vFinal[idim];
      plasmav2+=PlasmaBulkVelocity[idim]*PlasmaBulkVelocity[idim];
    }
    *((double*)(offset+OH::Output::ohSourceEnergyOffset)) += 
      c*0.5*_MASS_(_H_)*(v2-plasmav2); 
  }

  PIC::ParticleBuffer::SetX(xFinal,(PIC::ParticleBuffer::byte*)tempParticleData);
  PIC::ParticleBuffer::SetV(PlasmaBulkVelocity,(PIC::ParticleBuffer::byte*)tempParticleData);  
  PIC::ParticleBuffer::SetI(spec,(PIC::ParticleBuffer::byte*)tempParticleData);

#if _INDIVIDUAL_PARTICLE_WEIGHT_MODE_ == _INDIVIDUAL_PARTICLE_WEIGHT_ON_
  PIC::ParticleBuffer::SetIndividualStatWeightCorrection(ProductWeightCorrection,(PIC::ParticleBuffer::byte*)tempParticleData);
#endif

  //get and injection into the system the new model particle
  long int newParticle;
  PIC::ParticleBuffer::byte *newParticleData;
  newParticle=PIC::ParticleBuffer::GetNewParticle();
  newParticleData=PIC::ParticleBuffer::GetParticleDataPointer(newParticle);
  memcpy((void*)newParticleData,(void*)tempParticleData,PIC::ParticleBuffer::ParticleDataLength);
  
  _PIC_PARTICLE_MOVER__MOVE_PARTICLE_BOUNDARY_INJECTION_(newParticle,ParentTimeStep-TimeCounter,node,true);
  
  return _PHOTOLYTIC_REACTIONS_PARTICLE_REMOVED_;

}


void OH::Init_BeforeParser(){
  Exosphere::Init_BeforeParser();
  OH::Output::Init();

  //set the coupling procedure
  PIC::CPLR::SWMF::SendCenterPointData.push_back(Coupling::Send);
}


//-----------------------------------------------------------------------------
//substitutes for Exosphere functions
char Exosphere::ObjectName[_MAX_STRING_LENGTH_PIC_],Exosphere::IAU_FRAME[_MAX_STRING_LENGTH_PIC_],Exosphere::SO_FRAME[_MAX_STRING_LENGTH_PIC_];

//calcualte the true anomaly angle
double Exosphere::OrbitalMotion::GetTAA(SpiceDouble et) {
  return 0.0;
}

int Exosphere::ColumnIntegral::GetVariableList(char *vlist) {
  int spec,nVariables=0;

  //column density
  for (spec=0;spec<PIC::nTotalSpecies;spec++) {
    if (vlist!=NULL) sprintf(vlist,"%s,  \"Column Integral(%s)\"",vlist,PIC::MolecularData::GetChemSymbol(spec));
    nVariables+=1;
  }

  return nVariables;
}

void Exosphere::ColumnIntegral::CoulumnDensityIntegrant(double *res,int resLength,double* x,cTreeNodeAMR<PIC::Mesh::cDataBlockAMR>* node) {
  int i,j,k,nd,cnt=0,spec;
  double NumberDensity;


  nd=PIC::Mesh::mesh.fingCellIndex(x,i,j,k,node);
  for (i=0;i<resLength;i++) res[i]=0.0;

  for (spec=0;spec<PIC::nTotalSpecies;spec++) {
    //get the local density number
    NumberDensity=node->block->GetCenterNode(nd)->GetNumberDensity(spec);
    res[cnt++]=NumberDensity;
    //    res[cnt++]=NumberDensity*node->block->GetCenterNode(nd)->GetMeanParticleSpeed(spec);
  }


  if (cnt!=resLength) exit(__LINE__,__FILE__,"Error: the length of the vector is not coinsistent with the number of integrated variables");
}

void Exosphere::ColumnIntegral::ProcessColumnIntegrationVector(double *res,int resLength) {
  //do nothing
}

double Exosphere::SurfaceInteraction::StickingProbability(int spec,double& ReemissionParticleFraction,double Temp) {
  ReemissionParticleFraction=0.0;

  return 1.0;
}


double Exosphere::GetSurfaceTemeprature(double cosSubsolarAngle,double *x_LOCAL_SO_OBJECT) {


  return 100.0;
}
