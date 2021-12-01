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
-- Module      : Amazonka.EKS.CreateCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon EKS control plane.
--
-- The Amazon EKS control plane consists of control plane instances that
-- run the Kubernetes software, such as @etcd@ and the API server. The
-- control plane runs in an account managed by Amazon Web Services, and the
-- Kubernetes API is exposed via the Amazon EKS API server endpoint. Each
-- Amazon EKS cluster control plane is single-tenant and unique and runs on
-- its own set of Amazon EC2 instances.
--
-- The cluster control plane is provisioned across multiple Availability
-- Zones and fronted by an Elastic Load Balancing Network Load Balancer.
-- Amazon EKS also provisions elastic network interfaces in your VPC
-- subnets to provide connectivity from the control plane instances to the
-- nodes (for example, to support @kubectl exec@, @logs@, and @proxy@ data
-- flows).
--
-- Amazon EKS nodes run in your Amazon Web Services account and connect to
-- your cluster\'s control plane via the Kubernetes API server endpoint and
-- a certificate file that is created for your cluster.
--
-- Cluster creation typically takes several minutes. After you create an
-- Amazon EKS cluster, you must configure your Kubernetes tooling to
-- communicate with the API server and launch nodes into your cluster. For
-- more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/managing-auth.html Managing Cluster Authentication>
-- and
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-workers.html Launching Amazon EKS nodes>
-- in the /Amazon EKS User Guide/.
module Amazonka.EKS.CreateCluster
  ( -- * Creating a Request
    CreateCluster (..),
    newCreateCluster,

    -- * Request Lenses
    createCluster_kubernetesNetworkConfig,
    createCluster_version,
    createCluster_encryptionConfig,
    createCluster_clientRequestToken,
    createCluster_logging,
    createCluster_tags,
    createCluster_name,
    createCluster_roleArn,
    createCluster_resourcesVpcConfig,

    -- * Destructuring the Response
    CreateClusterResponse (..),
    newCreateClusterResponse,

    -- * Response Lenses
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EKS.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { -- | The Kubernetes network configuration for the cluster.
    kubernetesNetworkConfig :: Prelude.Maybe KubernetesNetworkConfigRequest,
    -- | The desired Kubernetes version for your cluster. If you don\'t specify a
    -- value here, the latest version available in Amazon EKS is used.
    version :: Prelude.Maybe Prelude.Text,
    -- | The encryption configuration for the cluster.
    encryptionConfig :: Prelude.Maybe [EncryptionConfig],
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Enable or disable exporting the Kubernetes control plane logs for your
    -- cluster to CloudWatch Logs. By default, cluster control plane logs
    -- aren\'t exported to CloudWatch Logs. For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/control-plane-logs.html Amazon EKS Cluster control plane logs>
    -- in the //Amazon EKS User Guide// .
    --
    -- CloudWatch Logs ingestion, archive storage, and data scanning rates
    -- apply to exported control plane logs. For more information, see
    -- <http://aws.amazon.com/cloudwatch/pricing/ CloudWatch Pricing>.
    logging :: Prelude.Maybe Logging,
    -- | The metadata to apply to the cluster to assist with categorization and
    -- organization. Each tag consists of a key and an optional value, both of
    -- which you define.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique name to give to your cluster.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that provides permissions
    -- for the Kubernetes control plane to make calls to Amazon Web Services
    -- API operations on your behalf. For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/service_IAM_role.html Amazon EKS Service IAM Role>
    -- in the //Amazon EKS User Guide// .
    roleArn :: Prelude.Text,
    -- | The VPC configuration used by the cluster control plane. Amazon EKS VPC
    -- resources have specific requirements to work properly with Kubernetes.
    -- For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/network_reqs.html Cluster VPC Considerations>
    -- and
    -- <https://docs.aws.amazon.com/eks/latest/userguide/sec-group-reqs.html Cluster Security Group Considerations>
    -- in the /Amazon EKS User Guide/. You must specify at least two subnets.
    -- You can specify up to five security groups, but we recommend that you
    -- use a dedicated security group for your cluster control plane.
    resourcesVpcConfig :: VpcConfigRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kubernetesNetworkConfig', 'createCluster_kubernetesNetworkConfig' - The Kubernetes network configuration for the cluster.
--
-- 'version', 'createCluster_version' - The desired Kubernetes version for your cluster. If you don\'t specify a
-- value here, the latest version available in Amazon EKS is used.
--
-- 'encryptionConfig', 'createCluster_encryptionConfig' - The encryption configuration for the cluster.
--
-- 'clientRequestToken', 'createCluster_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'logging', 'createCluster_logging' - Enable or disable exporting the Kubernetes control plane logs for your
-- cluster to CloudWatch Logs. By default, cluster control plane logs
-- aren\'t exported to CloudWatch Logs. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/control-plane-logs.html Amazon EKS Cluster control plane logs>
-- in the //Amazon EKS User Guide// .
--
-- CloudWatch Logs ingestion, archive storage, and data scanning rates
-- apply to exported control plane logs. For more information, see
-- <http://aws.amazon.com/cloudwatch/pricing/ CloudWatch Pricing>.
--
-- 'tags', 'createCluster_tags' - The metadata to apply to the cluster to assist with categorization and
-- organization. Each tag consists of a key and an optional value, both of
-- which you define.
--
-- 'name', 'createCluster_name' - The unique name to give to your cluster.
--
-- 'roleArn', 'createCluster_roleArn' - The Amazon Resource Name (ARN) of the IAM role that provides permissions
-- for the Kubernetes control plane to make calls to Amazon Web Services
-- API operations on your behalf. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/service_IAM_role.html Amazon EKS Service IAM Role>
-- in the //Amazon EKS User Guide// .
--
-- 'resourcesVpcConfig', 'createCluster_resourcesVpcConfig' - The VPC configuration used by the cluster control plane. Amazon EKS VPC
-- resources have specific requirements to work properly with Kubernetes.
-- For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/network_reqs.html Cluster VPC Considerations>
-- and
-- <https://docs.aws.amazon.com/eks/latest/userguide/sec-group-reqs.html Cluster Security Group Considerations>
-- in the /Amazon EKS User Guide/. You must specify at least two subnets.
-- You can specify up to five security groups, but we recommend that you
-- use a dedicated security group for your cluster control plane.
newCreateCluster ::
  -- | 'name'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'resourcesVpcConfig'
  VpcConfigRequest ->
  CreateCluster
newCreateCluster
  pName_
  pRoleArn_
  pResourcesVpcConfig_ =
    CreateCluster'
      { kubernetesNetworkConfig =
          Prelude.Nothing,
        version = Prelude.Nothing,
        encryptionConfig = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        logging = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        roleArn = pRoleArn_,
        resourcesVpcConfig = pResourcesVpcConfig_
      }

-- | The Kubernetes network configuration for the cluster.
createCluster_kubernetesNetworkConfig :: Lens.Lens' CreateCluster (Prelude.Maybe KubernetesNetworkConfigRequest)
createCluster_kubernetesNetworkConfig = Lens.lens (\CreateCluster' {kubernetesNetworkConfig} -> kubernetesNetworkConfig) (\s@CreateCluster' {} a -> s {kubernetesNetworkConfig = a} :: CreateCluster)

-- | The desired Kubernetes version for your cluster. If you don\'t specify a
-- value here, the latest version available in Amazon EKS is used.
createCluster_version :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_version = Lens.lens (\CreateCluster' {version} -> version) (\s@CreateCluster' {} a -> s {version = a} :: CreateCluster)

-- | The encryption configuration for the cluster.
createCluster_encryptionConfig :: Lens.Lens' CreateCluster (Prelude.Maybe [EncryptionConfig])
createCluster_encryptionConfig = Lens.lens (\CreateCluster' {encryptionConfig} -> encryptionConfig) (\s@CreateCluster' {} a -> s {encryptionConfig = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createCluster_clientRequestToken :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_clientRequestToken = Lens.lens (\CreateCluster' {clientRequestToken} -> clientRequestToken) (\s@CreateCluster' {} a -> s {clientRequestToken = a} :: CreateCluster)

-- | Enable or disable exporting the Kubernetes control plane logs for your
-- cluster to CloudWatch Logs. By default, cluster control plane logs
-- aren\'t exported to CloudWatch Logs. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/control-plane-logs.html Amazon EKS Cluster control plane logs>
-- in the //Amazon EKS User Guide// .
--
-- CloudWatch Logs ingestion, archive storage, and data scanning rates
-- apply to exported control plane logs. For more information, see
-- <http://aws.amazon.com/cloudwatch/pricing/ CloudWatch Pricing>.
createCluster_logging :: Lens.Lens' CreateCluster (Prelude.Maybe Logging)
createCluster_logging = Lens.lens (\CreateCluster' {logging} -> logging) (\s@CreateCluster' {} a -> s {logging = a} :: CreateCluster)

-- | The metadata to apply to the cluster to assist with categorization and
-- organization. Each tag consists of a key and an optional value, both of
-- which you define.
createCluster_tags :: Lens.Lens' CreateCluster (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCluster_tags = Lens.lens (\CreateCluster' {tags} -> tags) (\s@CreateCluster' {} a -> s {tags = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | The unique name to give to your cluster.
createCluster_name :: Lens.Lens' CreateCluster Prelude.Text
createCluster_name = Lens.lens (\CreateCluster' {name} -> name) (\s@CreateCluster' {} a -> s {name = a} :: CreateCluster)

-- | The Amazon Resource Name (ARN) of the IAM role that provides permissions
-- for the Kubernetes control plane to make calls to Amazon Web Services
-- API operations on your behalf. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/service_IAM_role.html Amazon EKS Service IAM Role>
-- in the //Amazon EKS User Guide// .
createCluster_roleArn :: Lens.Lens' CreateCluster Prelude.Text
createCluster_roleArn = Lens.lens (\CreateCluster' {roleArn} -> roleArn) (\s@CreateCluster' {} a -> s {roleArn = a} :: CreateCluster)

-- | The VPC configuration used by the cluster control plane. Amazon EKS VPC
-- resources have specific requirements to work properly with Kubernetes.
-- For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/network_reqs.html Cluster VPC Considerations>
-- and
-- <https://docs.aws.amazon.com/eks/latest/userguide/sec-group-reqs.html Cluster Security Group Considerations>
-- in the /Amazon EKS User Guide/. You must specify at least two subnets.
-- You can specify up to five security groups, but we recommend that you
-- use a dedicated security group for your cluster control plane.
createCluster_resourcesVpcConfig :: Lens.Lens' CreateCluster VpcConfigRequest
createCluster_resourcesVpcConfig = Lens.lens (\CreateCluster' {resourcesVpcConfig} -> resourcesVpcConfig) (\s@CreateCluster' {} a -> s {resourcesVpcConfig = a} :: CreateCluster)

instance Core.AWSRequest CreateCluster where
  type
    AWSResponse CreateCluster =
      CreateClusterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            Prelude.<$> (x Core..?> "cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCluster where
  hashWithSalt salt' CreateCluster' {..} =
    salt' `Prelude.hashWithSalt` resourcesVpcConfig
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` logging
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` encryptionConfig
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` kubernetesNetworkConfig

instance Prelude.NFData CreateCluster where
  rnf CreateCluster' {..} =
    Prelude.rnf kubernetesNetworkConfig
      `Prelude.seq` Prelude.rnf resourcesVpcConfig
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf logging
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf encryptionConfig
      `Prelude.seq` Prelude.rnf version

instance Core.ToHeaders CreateCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCluster where
  toJSON CreateCluster' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("kubernetesNetworkConfig" Core..=)
              Prelude.<$> kubernetesNetworkConfig,
            ("version" Core..=) Prelude.<$> version,
            ("encryptionConfig" Core..=)
              Prelude.<$> encryptionConfig,
            ("clientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("logging" Core..=) Prelude.<$> logging,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("roleArn" Core..= roleArn),
            Prelude.Just
              ("resourcesVpcConfig" Core..= resourcesVpcConfig)
          ]
      )

instance Core.ToPath CreateCluster where
  toPath = Prelude.const "/clusters"

instance Core.ToQuery CreateCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { -- | The full description of your new cluster.
    cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'createClusterResponse_cluster' - The full description of your new cluster.
--
-- 'httpStatus', 'createClusterResponse_httpStatus' - The response's http status code.
newCreateClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateClusterResponse
newCreateClusterResponse pHttpStatus_ =
  CreateClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of your new cluster.
createClusterResponse_cluster :: Lens.Lens' CreateClusterResponse (Prelude.Maybe Cluster)
createClusterResponse_cluster = Lens.lens (\CreateClusterResponse' {cluster} -> cluster) (\s@CreateClusterResponse' {} a -> s {cluster = a} :: CreateClusterResponse)

-- | The response's http status code.
createClusterResponse_httpStatus :: Lens.Lens' CreateClusterResponse Prelude.Int
createClusterResponse_httpStatus = Lens.lens (\CreateClusterResponse' {httpStatus} -> httpStatus) (\s@CreateClusterResponse' {} a -> s {httpStatus = a} :: CreateClusterResponse)

instance Prelude.NFData CreateClusterResponse where
  rnf CreateClusterResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
