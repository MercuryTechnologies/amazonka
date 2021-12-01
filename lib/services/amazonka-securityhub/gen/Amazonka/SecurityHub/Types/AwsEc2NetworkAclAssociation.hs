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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2NetworkAclAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2NetworkAclAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An association between the network ACL and a subnet.
--
-- /See:/ 'newAwsEc2NetworkAclAssociation' smart constructor.
data AwsEc2NetworkAclAssociation = AwsEc2NetworkAclAssociation'
  { -- | The identifier of the network ACL.
    networkAclId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the subnet that is associated with the network ACL.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the association between the network ACL and the
    -- subnet.
    networkAclAssociationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2NetworkAclAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkAclId', 'awsEc2NetworkAclAssociation_networkAclId' - The identifier of the network ACL.
--
-- 'subnetId', 'awsEc2NetworkAclAssociation_subnetId' - The identifier of the subnet that is associated with the network ACL.
--
-- 'networkAclAssociationId', 'awsEc2NetworkAclAssociation_networkAclAssociationId' - The identifier of the association between the network ACL and the
-- subnet.
newAwsEc2NetworkAclAssociation ::
  AwsEc2NetworkAclAssociation
newAwsEc2NetworkAclAssociation =
  AwsEc2NetworkAclAssociation'
    { networkAclId =
        Prelude.Nothing,
      subnetId = Prelude.Nothing,
      networkAclAssociationId = Prelude.Nothing
    }

-- | The identifier of the network ACL.
awsEc2NetworkAclAssociation_networkAclId :: Lens.Lens' AwsEc2NetworkAclAssociation (Prelude.Maybe Prelude.Text)
awsEc2NetworkAclAssociation_networkAclId = Lens.lens (\AwsEc2NetworkAclAssociation' {networkAclId} -> networkAclId) (\s@AwsEc2NetworkAclAssociation' {} a -> s {networkAclId = a} :: AwsEc2NetworkAclAssociation)

-- | The identifier of the subnet that is associated with the network ACL.
awsEc2NetworkAclAssociation_subnetId :: Lens.Lens' AwsEc2NetworkAclAssociation (Prelude.Maybe Prelude.Text)
awsEc2NetworkAclAssociation_subnetId = Lens.lens (\AwsEc2NetworkAclAssociation' {subnetId} -> subnetId) (\s@AwsEc2NetworkAclAssociation' {} a -> s {subnetId = a} :: AwsEc2NetworkAclAssociation)

-- | The identifier of the association between the network ACL and the
-- subnet.
awsEc2NetworkAclAssociation_networkAclAssociationId :: Lens.Lens' AwsEc2NetworkAclAssociation (Prelude.Maybe Prelude.Text)
awsEc2NetworkAclAssociation_networkAclAssociationId = Lens.lens (\AwsEc2NetworkAclAssociation' {networkAclAssociationId} -> networkAclAssociationId) (\s@AwsEc2NetworkAclAssociation' {} a -> s {networkAclAssociationId = a} :: AwsEc2NetworkAclAssociation)

instance Core.FromJSON AwsEc2NetworkAclAssociation where
  parseJSON =
    Core.withObject
      "AwsEc2NetworkAclAssociation"
      ( \x ->
          AwsEc2NetworkAclAssociation'
            Prelude.<$> (x Core..:? "NetworkAclId")
            Prelude.<*> (x Core..:? "SubnetId")
            Prelude.<*> (x Core..:? "NetworkAclAssociationId")
      )

instance Prelude.Hashable AwsEc2NetworkAclAssociation where
  hashWithSalt salt' AwsEc2NetworkAclAssociation' {..} =
    salt'
      `Prelude.hashWithSalt` networkAclAssociationId
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` networkAclId

instance Prelude.NFData AwsEc2NetworkAclAssociation where
  rnf AwsEc2NetworkAclAssociation' {..} =
    Prelude.rnf networkAclId
      `Prelude.seq` Prelude.rnf networkAclAssociationId
      `Prelude.seq` Prelude.rnf subnetId

instance Core.ToJSON AwsEc2NetworkAclAssociation where
  toJSON AwsEc2NetworkAclAssociation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NetworkAclId" Core..=) Prelude.<$> networkAclId,
            ("SubnetId" Core..=) Prelude.<$> subnetId,
            ("NetworkAclAssociationId" Core..=)
              Prelude.<$> networkAclAssociationId
          ]
      )
