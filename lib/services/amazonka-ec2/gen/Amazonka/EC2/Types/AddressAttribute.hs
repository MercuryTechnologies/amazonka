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
-- Module      : Amazonka.EC2.Types.AddressAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AddressAttribute where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PtrUpdateStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The attributes associated with an Elastic IP address.
--
-- /See:/ 'newAddressAttribute' smart constructor.
data AddressAttribute = AddressAttribute'
  { -- | The updated PTR record for the IP address.
    ptrRecordUpdate :: Prelude.Maybe PtrUpdateStatus,
    -- | [EC2-VPC] The allocation ID.
    allocationId :: Prelude.Maybe Prelude.Text,
    -- | The public IP address.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | The pointer (PTR) record for the IP address.
    ptrRecord :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddressAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ptrRecordUpdate', 'addressAttribute_ptrRecordUpdate' - The updated PTR record for the IP address.
--
-- 'allocationId', 'addressAttribute_allocationId' - [EC2-VPC] The allocation ID.
--
-- 'publicIp', 'addressAttribute_publicIp' - The public IP address.
--
-- 'ptrRecord', 'addressAttribute_ptrRecord' - The pointer (PTR) record for the IP address.
newAddressAttribute ::
  AddressAttribute
newAddressAttribute =
  AddressAttribute'
    { ptrRecordUpdate =
        Prelude.Nothing,
      allocationId = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      ptrRecord = Prelude.Nothing
    }

-- | The updated PTR record for the IP address.
addressAttribute_ptrRecordUpdate :: Lens.Lens' AddressAttribute (Prelude.Maybe PtrUpdateStatus)
addressAttribute_ptrRecordUpdate = Lens.lens (\AddressAttribute' {ptrRecordUpdate} -> ptrRecordUpdate) (\s@AddressAttribute' {} a -> s {ptrRecordUpdate = a} :: AddressAttribute)

-- | [EC2-VPC] The allocation ID.
addressAttribute_allocationId :: Lens.Lens' AddressAttribute (Prelude.Maybe Prelude.Text)
addressAttribute_allocationId = Lens.lens (\AddressAttribute' {allocationId} -> allocationId) (\s@AddressAttribute' {} a -> s {allocationId = a} :: AddressAttribute)

-- | The public IP address.
addressAttribute_publicIp :: Lens.Lens' AddressAttribute (Prelude.Maybe Prelude.Text)
addressAttribute_publicIp = Lens.lens (\AddressAttribute' {publicIp} -> publicIp) (\s@AddressAttribute' {} a -> s {publicIp = a} :: AddressAttribute)

-- | The pointer (PTR) record for the IP address.
addressAttribute_ptrRecord :: Lens.Lens' AddressAttribute (Prelude.Maybe Prelude.Text)
addressAttribute_ptrRecord = Lens.lens (\AddressAttribute' {ptrRecord} -> ptrRecord) (\s@AddressAttribute' {} a -> s {ptrRecord = a} :: AddressAttribute)

instance Core.FromXML AddressAttribute where
  parseXML x =
    AddressAttribute'
      Prelude.<$> (x Core..@? "ptrRecordUpdate")
      Prelude.<*> (x Core..@? "allocationId")
      Prelude.<*> (x Core..@? "publicIp")
      Prelude.<*> (x Core..@? "ptrRecord")

instance Prelude.Hashable AddressAttribute where
  hashWithSalt salt' AddressAttribute' {..} =
    salt' `Prelude.hashWithSalt` ptrRecord
      `Prelude.hashWithSalt` publicIp
      `Prelude.hashWithSalt` allocationId
      `Prelude.hashWithSalt` ptrRecordUpdate

instance Prelude.NFData AddressAttribute where
  rnf AddressAttribute' {..} =
    Prelude.rnf ptrRecordUpdate
      `Prelude.seq` Prelude.rnf ptrRecord
      `Prelude.seq` Prelude.rnf publicIp
      `Prelude.seq` Prelude.rnf allocationId
