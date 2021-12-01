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
-- Module      : Amazonka.EC2.Types.NetworkAcl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkAcl where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.NetworkAclAssociation
import Amazonka.EC2.Types.NetworkAclEntry
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a network ACL.
--
-- /See:/ 'newNetworkAcl' smart constructor.
data NetworkAcl = NetworkAcl'
  { -- | One or more entries (rules) in the network ACL.
    entries :: Prelude.Maybe [NetworkAclEntry],
    -- | The ID of the network ACL.
    networkAclId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC for the network ACL.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the network ACL.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Any associations between the network ACL and one or more subnets
    associations :: Prelude.Maybe [NetworkAclAssociation],
    -- | Any tags assigned to the network ACL.
    tags :: Prelude.Maybe [Tag],
    -- | Indicates whether this is the default network ACL for the VPC.
    isDefault :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkAcl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entries', 'networkAcl_entries' - One or more entries (rules) in the network ACL.
--
-- 'networkAclId', 'networkAcl_networkAclId' - The ID of the network ACL.
--
-- 'vpcId', 'networkAcl_vpcId' - The ID of the VPC for the network ACL.
--
-- 'ownerId', 'networkAcl_ownerId' - The ID of the Amazon Web Services account that owns the network ACL.
--
-- 'associations', 'networkAcl_associations' - Any associations between the network ACL and one or more subnets
--
-- 'tags', 'networkAcl_tags' - Any tags assigned to the network ACL.
--
-- 'isDefault', 'networkAcl_isDefault' - Indicates whether this is the default network ACL for the VPC.
newNetworkAcl ::
  NetworkAcl
newNetworkAcl =
  NetworkAcl'
    { entries = Prelude.Nothing,
      networkAclId = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      associations = Prelude.Nothing,
      tags = Prelude.Nothing,
      isDefault = Prelude.Nothing
    }

-- | One or more entries (rules) in the network ACL.
networkAcl_entries :: Lens.Lens' NetworkAcl (Prelude.Maybe [NetworkAclEntry])
networkAcl_entries = Lens.lens (\NetworkAcl' {entries} -> entries) (\s@NetworkAcl' {} a -> s {entries = a} :: NetworkAcl) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the network ACL.
networkAcl_networkAclId :: Lens.Lens' NetworkAcl (Prelude.Maybe Prelude.Text)
networkAcl_networkAclId = Lens.lens (\NetworkAcl' {networkAclId} -> networkAclId) (\s@NetworkAcl' {} a -> s {networkAclId = a} :: NetworkAcl)

-- | The ID of the VPC for the network ACL.
networkAcl_vpcId :: Lens.Lens' NetworkAcl (Prelude.Maybe Prelude.Text)
networkAcl_vpcId = Lens.lens (\NetworkAcl' {vpcId} -> vpcId) (\s@NetworkAcl' {} a -> s {vpcId = a} :: NetworkAcl)

-- | The ID of the Amazon Web Services account that owns the network ACL.
networkAcl_ownerId :: Lens.Lens' NetworkAcl (Prelude.Maybe Prelude.Text)
networkAcl_ownerId = Lens.lens (\NetworkAcl' {ownerId} -> ownerId) (\s@NetworkAcl' {} a -> s {ownerId = a} :: NetworkAcl)

-- | Any associations between the network ACL and one or more subnets
networkAcl_associations :: Lens.Lens' NetworkAcl (Prelude.Maybe [NetworkAclAssociation])
networkAcl_associations = Lens.lens (\NetworkAcl' {associations} -> associations) (\s@NetworkAcl' {} a -> s {associations = a} :: NetworkAcl) Prelude.. Lens.mapping Lens.coerced

-- | Any tags assigned to the network ACL.
networkAcl_tags :: Lens.Lens' NetworkAcl (Prelude.Maybe [Tag])
networkAcl_tags = Lens.lens (\NetworkAcl' {tags} -> tags) (\s@NetworkAcl' {} a -> s {tags = a} :: NetworkAcl) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether this is the default network ACL for the VPC.
networkAcl_isDefault :: Lens.Lens' NetworkAcl (Prelude.Maybe Prelude.Bool)
networkAcl_isDefault = Lens.lens (\NetworkAcl' {isDefault} -> isDefault) (\s@NetworkAcl' {} a -> s {isDefault = a} :: NetworkAcl)

instance Core.FromXML NetworkAcl where
  parseXML x =
    NetworkAcl'
      Prelude.<$> ( x Core..@? "entrySet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "networkAclId")
      Prelude.<*> (x Core..@? "vpcId")
      Prelude.<*> (x Core..@? "ownerId")
      Prelude.<*> ( x Core..@? "associationSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "default")

instance Prelude.Hashable NetworkAcl where
  hashWithSalt salt' NetworkAcl' {..} =
    salt' `Prelude.hashWithSalt` isDefault
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` associations
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` networkAclId
      `Prelude.hashWithSalt` entries

instance Prelude.NFData NetworkAcl where
  rnf NetworkAcl' {..} =
    Prelude.rnf entries
      `Prelude.seq` Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf associations
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf networkAclId
