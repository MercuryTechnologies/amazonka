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
-- Module      : Amazonka.Redshift.Types.VpcEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.VpcEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.NetworkInterface

-- | The connection endpoint for connecting to an Amazon Redshift cluster
-- through the proxy.
--
-- /See:/ 'newVpcEndpoint' smart constructor.
data VpcEndpoint = VpcEndpoint'
  { -- | The VPC identifier that the endpoint is associated.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | One or more network interfaces of the endpoint. Also known as an
    -- interface endpoint.
    networkInterfaces :: Prelude.Maybe [NetworkInterface],
    -- | The connection endpoint ID for connecting an Amazon Redshift cluster
    -- through the proxy.
    vpcEndpointId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcId', 'vpcEndpoint_vpcId' - The VPC identifier that the endpoint is associated.
--
-- 'networkInterfaces', 'vpcEndpoint_networkInterfaces' - One or more network interfaces of the endpoint. Also known as an
-- interface endpoint.
--
-- 'vpcEndpointId', 'vpcEndpoint_vpcEndpointId' - The connection endpoint ID for connecting an Amazon Redshift cluster
-- through the proxy.
newVpcEndpoint ::
  VpcEndpoint
newVpcEndpoint =
  VpcEndpoint'
    { vpcId = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      vpcEndpointId = Prelude.Nothing
    }

-- | The VPC identifier that the endpoint is associated.
vpcEndpoint_vpcId :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Text)
vpcEndpoint_vpcId = Lens.lens (\VpcEndpoint' {vpcId} -> vpcId) (\s@VpcEndpoint' {} a -> s {vpcId = a} :: VpcEndpoint)

-- | One or more network interfaces of the endpoint. Also known as an
-- interface endpoint.
vpcEndpoint_networkInterfaces :: Lens.Lens' VpcEndpoint (Prelude.Maybe [NetworkInterface])
vpcEndpoint_networkInterfaces = Lens.lens (\VpcEndpoint' {networkInterfaces} -> networkInterfaces) (\s@VpcEndpoint' {} a -> s {networkInterfaces = a} :: VpcEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The connection endpoint ID for connecting an Amazon Redshift cluster
-- through the proxy.
vpcEndpoint_vpcEndpointId :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Text)
vpcEndpoint_vpcEndpointId = Lens.lens (\VpcEndpoint' {vpcEndpointId} -> vpcEndpointId) (\s@VpcEndpoint' {} a -> s {vpcEndpointId = a} :: VpcEndpoint)

instance Core.FromXML VpcEndpoint where
  parseXML x =
    VpcEndpoint'
      Prelude.<$> (x Core..@? "VpcId")
      Prelude.<*> ( x Core..@? "NetworkInterfaces"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "NetworkInterface")
                  )
      Prelude.<*> (x Core..@? "VpcEndpointId")

instance Prelude.Hashable VpcEndpoint where
  hashWithSalt salt' VpcEndpoint' {..} =
    salt' `Prelude.hashWithSalt` vpcEndpointId
      `Prelude.hashWithSalt` networkInterfaces
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData VpcEndpoint where
  rnf VpcEndpoint' {..} =
    Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf vpcEndpointId
      `Prelude.seq` Prelude.rnf networkInterfaces
