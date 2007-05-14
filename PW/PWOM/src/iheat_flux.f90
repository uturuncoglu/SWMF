SUBROUTINE PW_iheat_flux
  
  use ModCommonVariables
  use ModCommonPlanet,ONLY: nIon
  
  REAL C1(MaxGrid),C2(MaxGrid),C3(MaxGrid),C4(MaxGrid),&
       D1(MaxGrid),D2(MaxGrid),&
       D3(MaxGrid),D4(MaxGrid)
  REAL YL(MaxGrid),YK(MaxGrid)
  
  do iIon=1,nIon-1
     
     C2(1)=H0*(HeatCon_GI(2,iIon)-HeatCon_GI(0,iIon))
     C2(NDIM)=H0*(HeatCon_GI(nDim+1,iIon)-HeatCon_GI(NDIM2,iIon))
     DO  K=2,NDIM2
        C2(K)=H0*(HeatCon_GI(K+1,iIon)-HeatCon_GI(K-1,iIon))
     enddo
     DO K=1,NDIM
      C1(K)=HeatCon_GI(K,iIon)/State_GV(K,iRho_I(iIon))
      C2(K)=C2(K)/State_GV(K,iRho_I(iIon))
      C2(K)=C2(K)+DAREA(K)*C1(K)
   enddo
   
   DO K=1,NDIM
      XX1=H3*C1(K)
      XX2=H4*C2(K)
      XX3=H2*C1(K)
      XX4=-XX3
      D1(K)=-XX1-XX2
      D2(K)=H1O2-XX4
      D3(K)=-XX1+XX2
      D4(K)=H1O2+XX4
   enddo
   D4(1)=&
        -D3(1)*State_GV(0,iT_I(iIon))+D4(1)*State_GV(1,iT_I(iIon))&
        -D1(1)*State_GV(2,iT_I(iIon))
   D4(NDIM)=-D3(NDIM)*State_GV(nDim2,iT_I(iIon))+&
        D4(NDIM)*State_GV(nDim,iT_I(iIon))-D1(NDIM)*State_GV(nDim+1,iT_I(iIon))
      DO K=2,NDIM2
         D4(K)=-D3(K)*State_GV(K-1,iT_I(iIon))+D4(K)*State_GV(K,iT_I(iIon))-&
              D1(K)*State_GV(K+1,iT_I(iIon))
      enddo
      YL(1)=D1(1)/D2(1)
      YK(1)=(D4(1)-D3(1)*State_GV(0,iT_I(iIon)))/D2(1)
      DO  K=2,NDIM
         XX1=D2(K)-D3(K)*YL(K-1)
         YK(K)=(D4(K)-D3(K)*YK(K-1))/XX1
         YL(K)=D1(K)/XX1
      enddo
      State_GV(nDim,iT_I(iIon))=YK(NDIM)-YL(NDIM)*State_GV(nDim+1,iT_I(iIon))
      State_GV(nDim,iP_I(iIon))=&
           RGAS_I(iIon)*State_GV(nDim,iRho_I(iIon))*State_GV(nDim,iT_I(iIon))
      DO  K=NDIM2,1,-1
         State_GV(K,iT_I(iIon))=YK(K)-YL(K)*State_GV(K+1,iT_I(iIon))
         State_GV(K,iP_I(iIon))=&
              RGAS_I(iIon)*State_GV(K,iRho_I(iIon))*State_GV(K,iT_I(iIon))
      enddo
   enddo
   RETURN
 END SUBROUTINE PW_iheat_flux
