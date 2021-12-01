{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53RecoveryControlConfig.Types.ClusterEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryControlConfig.Types.ClusterEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A cluster endpoint. Specify an endpoint when you want to set or retrieve
-- a routing control state in the cluster.
--
-- /See:/ 'newClusterEndpoint' smart constructor.
data ClusterEndpoint = ClusterEndpoint'
  { -- | The Amazon Web Services Region for a cluster endpoint.
    region :: Prelude.Maybe Prelude.Text,
    -- | A cluster endpoint. Specify an endpoint and Amazon Web Services Region
    -- when you want to set or retrieve a routing control state in the cluster.
    --
    -- To get or update the routing control state, see the Amazon Route 53
    -- Application Recovery Controller Cluster (Data Plane) Actions.
    endpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'region', 'clusterEndpoint_region' - The Amazon Web Services Region for a cluster endpoint.
--
-- 'endpoint', 'clusterEndpoint_endpoint' - A cluster endpoint. Specify an endpoint and Amazon Web Services Region
-- when you want to set or retrieve a routing control state in the cluster.
--
-- To get or update the routing control state, see the Amazon Route 53
-- Application Recovery Controller Cluster (Data Plane) Actions.
newClusterEndpoint ::
  ClusterEndpoint
newClusterEndpoint =
  ClusterEndpoint'
    { region = Prelude.Nothing,
      endpoint = Prelude.Nothing
    }

-- | The Amazon Web Services Region for a cluster endpoint.
clusterEndpoint_region :: Lens.Lens' ClusterEndpoint (Prelude.Maybe Prelude.Text)
clusterEndpoint_region = Lens.lens (\ClusterEndpoint' {region} -> region) (\s@ClusterEndpoint' {} a -> s {region = a} :: ClusterEndpoint)

-- | A cluster endpoint. Specify an endpoint and Amazon Web Services Region
-- when you want to set or retrieve a routing control state in the cluster.
--
-- To get or update the routing control state, see the Amazon Route 53
-- Application Recovery Controller Cluster (Data Plane) Actions.
clusterEndpoint_endpoint :: Lens.Lens' ClusterEndpoint (Prelude.Maybe Prelude.Text)
clusterEndpoint_endpoint = Lens.lens (\ClusterEndpoint' {endpoint} -> endpoint) (\s@ClusterEndpoint' {} a -> s {endpoint = a} :: ClusterEndpoint)

instance Core.FromJSON ClusterEndpoint where
  parseJSON =
    Core.withObject
      "ClusterEndpoint"
      ( \x ->
          ClusterEndpoint'
            Prelude.<$> (x Core..:? "Region")
            Prelude.<*> (x Core..:? "Endpoint")
      )

instance Prelude.Hashable ClusterEndpoint where
  hashWithSalt salt' ClusterEndpoint' {..} =
    salt' `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` region

instance Prelude.NFData ClusterEndpoint where
  rnf ClusterEndpoint' {..} =
    Prelude.rnf region
      `Prelude.seq` Prelude.rnf endpoint
