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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2NetworkAclDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2NetworkAclDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2NetworkAclAssociation
import Amazonka.SecurityHub.Types.AwsEc2NetworkAclEntry

-- | Contains details about an EC2 network access control list (ACL).
--
-- /See:/ 'newAwsEc2NetworkAclDetails' smart constructor.
data AwsEc2NetworkAclDetails = AwsEc2NetworkAclDetails'
  { -- | The set of rules in the network ACL.
    entries :: Prelude.Maybe [AwsEc2NetworkAclEntry],
    -- | The identifier of the network ACL.
    networkAclId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the VPC for the network ACL.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Web Services account that owns the network
    -- ACL.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Associations between the network ACL and subnets.
    associations :: Prelude.Maybe [AwsEc2NetworkAclAssociation],
    -- | Whether this is the default network ACL for the VPC.
    isDefault :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2NetworkAclDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entries', 'awsEc2NetworkAclDetails_entries' - The set of rules in the network ACL.
--
-- 'networkAclId', 'awsEc2NetworkAclDetails_networkAclId' - The identifier of the network ACL.
--
-- 'vpcId', 'awsEc2NetworkAclDetails_vpcId' - The identifier of the VPC for the network ACL.
--
-- 'ownerId', 'awsEc2NetworkAclDetails_ownerId' - The identifier of the Amazon Web Services account that owns the network
-- ACL.
--
-- 'associations', 'awsEc2NetworkAclDetails_associations' - Associations between the network ACL and subnets.
--
-- 'isDefault', 'awsEc2NetworkAclDetails_isDefault' - Whether this is the default network ACL for the VPC.
newAwsEc2NetworkAclDetails ::
  AwsEc2NetworkAclDetails
newAwsEc2NetworkAclDetails =
  AwsEc2NetworkAclDetails'
    { entries = Prelude.Nothing,
      networkAclId = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      associations = Prelude.Nothing,
      isDefault = Prelude.Nothing
    }

-- | The set of rules in the network ACL.
awsEc2NetworkAclDetails_entries :: Lens.Lens' AwsEc2NetworkAclDetails (Prelude.Maybe [AwsEc2NetworkAclEntry])
awsEc2NetworkAclDetails_entries = Lens.lens (\AwsEc2NetworkAclDetails' {entries} -> entries) (\s@AwsEc2NetworkAclDetails' {} a -> s {entries = a} :: AwsEc2NetworkAclDetails) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the network ACL.
awsEc2NetworkAclDetails_networkAclId :: Lens.Lens' AwsEc2NetworkAclDetails (Prelude.Maybe Prelude.Text)
awsEc2NetworkAclDetails_networkAclId = Lens.lens (\AwsEc2NetworkAclDetails' {networkAclId} -> networkAclId) (\s@AwsEc2NetworkAclDetails' {} a -> s {networkAclId = a} :: AwsEc2NetworkAclDetails)

-- | The identifier of the VPC for the network ACL.
awsEc2NetworkAclDetails_vpcId :: Lens.Lens' AwsEc2NetworkAclDetails (Prelude.Maybe Prelude.Text)
awsEc2NetworkAclDetails_vpcId = Lens.lens (\AwsEc2NetworkAclDetails' {vpcId} -> vpcId) (\s@AwsEc2NetworkAclDetails' {} a -> s {vpcId = a} :: AwsEc2NetworkAclDetails)

-- | The identifier of the Amazon Web Services account that owns the network
-- ACL.
awsEc2NetworkAclDetails_ownerId :: Lens.Lens' AwsEc2NetworkAclDetails (Prelude.Maybe Prelude.Text)
awsEc2NetworkAclDetails_ownerId = Lens.lens (\AwsEc2NetworkAclDetails' {ownerId} -> ownerId) (\s@AwsEc2NetworkAclDetails' {} a -> s {ownerId = a} :: AwsEc2NetworkAclDetails)

-- | Associations between the network ACL and subnets.
awsEc2NetworkAclDetails_associations :: Lens.Lens' AwsEc2NetworkAclDetails (Prelude.Maybe [AwsEc2NetworkAclAssociation])
awsEc2NetworkAclDetails_associations = Lens.lens (\AwsEc2NetworkAclDetails' {associations} -> associations) (\s@AwsEc2NetworkAclDetails' {} a -> s {associations = a} :: AwsEc2NetworkAclDetails) Prelude.. Lens.mapping Lens.coerced

-- | Whether this is the default network ACL for the VPC.
awsEc2NetworkAclDetails_isDefault :: Lens.Lens' AwsEc2NetworkAclDetails (Prelude.Maybe Prelude.Bool)
awsEc2NetworkAclDetails_isDefault = Lens.lens (\AwsEc2NetworkAclDetails' {isDefault} -> isDefault) (\s@AwsEc2NetworkAclDetails' {} a -> s {isDefault = a} :: AwsEc2NetworkAclDetails)

instance Core.FromJSON AwsEc2NetworkAclDetails where
  parseJSON =
    Core.withObject
      "AwsEc2NetworkAclDetails"
      ( \x ->
          AwsEc2NetworkAclDetails'
            Prelude.<$> (x Core..:? "Entries" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "NetworkAclId")
            Prelude.<*> (x Core..:? "VpcId")
            Prelude.<*> (x Core..:? "OwnerId")
            Prelude.<*> (x Core..:? "Associations" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "IsDefault")
      )

instance Prelude.Hashable AwsEc2NetworkAclDetails where
  hashWithSalt salt' AwsEc2NetworkAclDetails' {..} =
    salt' `Prelude.hashWithSalt` isDefault
      `Prelude.hashWithSalt` associations
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` networkAclId
      `Prelude.hashWithSalt` entries

instance Prelude.NFData AwsEc2NetworkAclDetails where
  rnf AwsEc2NetworkAclDetails' {..} =
    Prelude.rnf entries
      `Prelude.seq` Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf associations
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf networkAclId

instance Core.ToJSON AwsEc2NetworkAclDetails where
  toJSON AwsEc2NetworkAclDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Entries" Core..=) Prelude.<$> entries,
            ("NetworkAclId" Core..=) Prelude.<$> networkAclId,
            ("VpcId" Core..=) Prelude.<$> vpcId,
            ("OwnerId" Core..=) Prelude.<$> ownerId,
            ("Associations" Core..=) Prelude.<$> associations,
            ("IsDefault" Core..=) Prelude.<$> isDefault
          ]
      )
