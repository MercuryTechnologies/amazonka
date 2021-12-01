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
-- Module      : Amazonka.EC2.Types.VpcCidrBlockAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpcCidrBlockAssociation where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VpcCidrBlockState
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an IPv4 CIDR block associated with a VPC.
--
-- /See:/ 'newVpcCidrBlockAssociation' smart constructor.
data VpcCidrBlockAssociation = VpcCidrBlockAssociation'
  { -- | The association ID for the IPv4 CIDR block.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | Information about the state of the CIDR block.
    cidrBlockState :: Prelude.Maybe VpcCidrBlockState,
    -- | The IPv4 CIDR block.
    cidrBlock :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcCidrBlockAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'vpcCidrBlockAssociation_associationId' - The association ID for the IPv4 CIDR block.
--
-- 'cidrBlockState', 'vpcCidrBlockAssociation_cidrBlockState' - Information about the state of the CIDR block.
--
-- 'cidrBlock', 'vpcCidrBlockAssociation_cidrBlock' - The IPv4 CIDR block.
newVpcCidrBlockAssociation ::
  VpcCidrBlockAssociation
newVpcCidrBlockAssociation =
  VpcCidrBlockAssociation'
    { associationId =
        Prelude.Nothing,
      cidrBlockState = Prelude.Nothing,
      cidrBlock = Prelude.Nothing
    }

-- | The association ID for the IPv4 CIDR block.
vpcCidrBlockAssociation_associationId :: Lens.Lens' VpcCidrBlockAssociation (Prelude.Maybe Prelude.Text)
vpcCidrBlockAssociation_associationId = Lens.lens (\VpcCidrBlockAssociation' {associationId} -> associationId) (\s@VpcCidrBlockAssociation' {} a -> s {associationId = a} :: VpcCidrBlockAssociation)

-- | Information about the state of the CIDR block.
vpcCidrBlockAssociation_cidrBlockState :: Lens.Lens' VpcCidrBlockAssociation (Prelude.Maybe VpcCidrBlockState)
vpcCidrBlockAssociation_cidrBlockState = Lens.lens (\VpcCidrBlockAssociation' {cidrBlockState} -> cidrBlockState) (\s@VpcCidrBlockAssociation' {} a -> s {cidrBlockState = a} :: VpcCidrBlockAssociation)

-- | The IPv4 CIDR block.
vpcCidrBlockAssociation_cidrBlock :: Lens.Lens' VpcCidrBlockAssociation (Prelude.Maybe Prelude.Text)
vpcCidrBlockAssociation_cidrBlock = Lens.lens (\VpcCidrBlockAssociation' {cidrBlock} -> cidrBlock) (\s@VpcCidrBlockAssociation' {} a -> s {cidrBlock = a} :: VpcCidrBlockAssociation)

instance Core.FromXML VpcCidrBlockAssociation where
  parseXML x =
    VpcCidrBlockAssociation'
      Prelude.<$> (x Core..@? "associationId")
      Prelude.<*> (x Core..@? "cidrBlockState")
      Prelude.<*> (x Core..@? "cidrBlock")

instance Prelude.Hashable VpcCidrBlockAssociation where
  hashWithSalt salt' VpcCidrBlockAssociation' {..} =
    salt' `Prelude.hashWithSalt` cidrBlock
      `Prelude.hashWithSalt` cidrBlockState
      `Prelude.hashWithSalt` associationId

instance Prelude.NFData VpcCidrBlockAssociation where
  rnf VpcCidrBlockAssociation' {..} =
    Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf cidrBlock
      `Prelude.seq` Prelude.rnf cidrBlockState
