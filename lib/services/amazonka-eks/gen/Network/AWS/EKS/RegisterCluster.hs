{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EKS.RegisterCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Connects a Kubernetes cluster to the Amazon EKS control plane.
--
-- Any Kubernetes cluster can be connected to the Amazon EKS control plane
-- to view current information about the cluster and its nodes.
--
-- Cluster connection requires two steps. First, send a
-- @ RegisterClusterRequest @ to add it to the Amazon EKS control plane.
--
-- Second, a
-- <https://amazon-eks.s3.us-west-2.amazonaws.com/eks-connector/manifests/eks-connector/latest/eks-connector.yaml Manifest>
-- containing the @activationID@ and @activationCode@ must be applied to
-- the Kubernetes cluster through it\'s native provider to provide
-- visibility.
--
-- After the Manifest is updated and applied, then the connected cluster is
-- visible to the Amazon EKS control plane. If the Manifest is not applied
-- within a set amount of time, then the connected cluster will no longer
-- be visible and must be deregistered. See DeregisterCluster.
module Amazonka.EKS.RegisterCluster
  ( -- * Creating a Request
    RegisterCluster (..),
    newRegisterCluster,

    -- * Request Lenses
    registerCluster_clientRequestToken,
    registerCluster_name,
    registerCluster_connectorConfig,

    -- * Destructuring the Response
    RegisterClusterResponse (..),
    newRegisterClusterResponse,

    -- * Response Lenses
    registerClusterResponse_cluster,
    registerClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EKS.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterCluster' smart constructor.
data RegisterCluster = RegisterCluster'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Define a unique name for this cluster within your AWS account.
    name :: Prelude.Text,
    -- | The configuration settings required to connect the Kubernetes cluster to
    -- the Amazon EKS control plane.
    connectorConfig :: ConnectorConfigRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'registerCluster_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'name', 'registerCluster_name' - Define a unique name for this cluster within your AWS account.
--
-- 'connectorConfig', 'registerCluster_connectorConfig' - The configuration settings required to connect the Kubernetes cluster to
-- the Amazon EKS control plane.
newRegisterCluster ::
  -- | 'name'
  Prelude.Text ->
  -- | 'connectorConfig'
  ConnectorConfigRequest ->
  RegisterCluster
newRegisterCluster pName_ pConnectorConfig_ =
  RegisterCluster'
    { clientRequestToken =
        Prelude.Nothing,
      name = pName_,
      connectorConfig = pConnectorConfig_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
registerCluster_clientRequestToken :: Lens.Lens' RegisterCluster (Prelude.Maybe Prelude.Text)
registerCluster_clientRequestToken = Lens.lens (\RegisterCluster' {clientRequestToken} -> clientRequestToken) (\s@RegisterCluster' {} a -> s {clientRequestToken = a} :: RegisterCluster)

-- | Define a unique name for this cluster within your AWS account.
registerCluster_name :: Lens.Lens' RegisterCluster Prelude.Text
registerCluster_name = Lens.lens (\RegisterCluster' {name} -> name) (\s@RegisterCluster' {} a -> s {name = a} :: RegisterCluster)

-- | The configuration settings required to connect the Kubernetes cluster to
-- the Amazon EKS control plane.
registerCluster_connectorConfig :: Lens.Lens' RegisterCluster ConnectorConfigRequest
registerCluster_connectorConfig = Lens.lens (\RegisterCluster' {connectorConfig} -> connectorConfig) (\s@RegisterCluster' {} a -> s {connectorConfig = a} :: RegisterCluster)

instance Core.AWSRequest RegisterCluster where
  type
    AWSResponse RegisterCluster =
      RegisterClusterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterClusterResponse'
            Prelude.<$> (x Core..?> "cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterCluster

instance Prelude.NFData RegisterCluster

instance Core.ToHeaders RegisterCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RegisterCluster where
  toJSON RegisterCluster' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("name" Core..= name),
            Prelude.Just
              ("connectorConfig" Core..= connectorConfig)
          ]
      )

instance Core.ToPath RegisterCluster where
  toPath = Prelude.const "/cluster-registrations"

instance Core.ToQuery RegisterCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterClusterResponse' smart constructor.
data RegisterClusterResponse = RegisterClusterResponse'
  { cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'registerClusterResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'registerClusterResponse_httpStatus' - The response's http status code.
newRegisterClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterClusterResponse
newRegisterClusterResponse pHttpStatus_ =
  RegisterClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
registerClusterResponse_cluster :: Lens.Lens' RegisterClusterResponse (Prelude.Maybe Cluster)
registerClusterResponse_cluster = Lens.lens (\RegisterClusterResponse' {cluster} -> cluster) (\s@RegisterClusterResponse' {} a -> s {cluster = a} :: RegisterClusterResponse)

-- | The response's http status code.
registerClusterResponse_httpStatus :: Lens.Lens' RegisterClusterResponse Prelude.Int
registerClusterResponse_httpStatus = Lens.lens (\RegisterClusterResponse' {httpStatus} -> httpStatus) (\s@RegisterClusterResponse' {} a -> s {httpStatus = a} :: RegisterClusterResponse)

instance Prelude.NFData RegisterClusterResponse
