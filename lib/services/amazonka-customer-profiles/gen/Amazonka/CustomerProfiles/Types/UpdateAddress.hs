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
-- Module      : Amazonka.CustomerProfiles.Types.UpdateAddress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.UpdateAddress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Updates associated with the address properties of a customer profile.
--
-- /See:/ 'newUpdateAddress' smart constructor.
data UpdateAddress = UpdateAddress'
  { -- | The second line of a customer address.
    address2 :: Prelude.Maybe Prelude.Text,
    -- | The state in which a customer lives.
    state :: Prelude.Maybe Prelude.Text,
    -- | The fourth line of a customer address.
    address4 :: Prelude.Maybe Prelude.Text,
    -- | The county in which a customer lives.
    county :: Prelude.Maybe Prelude.Text,
    -- | The third line of a customer address.
    address3 :: Prelude.Maybe Prelude.Text,
    -- | The postal code of a customer address.
    postalCode :: Prelude.Maybe Prelude.Text,
    -- | The country in which a customer lives.
    country :: Prelude.Maybe Prelude.Text,
    -- | The city in which a customer lives.
    city :: Prelude.Maybe Prelude.Text,
    -- | The first line of a customer address.
    address1 :: Prelude.Maybe Prelude.Text,
    -- | The province in which a customer lives.
    province :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address2', 'updateAddress_address2' - The second line of a customer address.
--
-- 'state', 'updateAddress_state' - The state in which a customer lives.
--
-- 'address4', 'updateAddress_address4' - The fourth line of a customer address.
--
-- 'county', 'updateAddress_county' - The county in which a customer lives.
--
-- 'address3', 'updateAddress_address3' - The third line of a customer address.
--
-- 'postalCode', 'updateAddress_postalCode' - The postal code of a customer address.
--
-- 'country', 'updateAddress_country' - The country in which a customer lives.
--
-- 'city', 'updateAddress_city' - The city in which a customer lives.
--
-- 'address1', 'updateAddress_address1' - The first line of a customer address.
--
-- 'province', 'updateAddress_province' - The province in which a customer lives.
newUpdateAddress ::
  UpdateAddress
newUpdateAddress =
  UpdateAddress'
    { address2 = Prelude.Nothing,
      state = Prelude.Nothing,
      address4 = Prelude.Nothing,
      county = Prelude.Nothing,
      address3 = Prelude.Nothing,
      postalCode = Prelude.Nothing,
      country = Prelude.Nothing,
      city = Prelude.Nothing,
      address1 = Prelude.Nothing,
      province = Prelude.Nothing
    }

-- | The second line of a customer address.
updateAddress_address2 :: Lens.Lens' UpdateAddress (Prelude.Maybe Prelude.Text)
updateAddress_address2 = Lens.lens (\UpdateAddress' {address2} -> address2) (\s@UpdateAddress' {} a -> s {address2 = a} :: UpdateAddress)

-- | The state in which a customer lives.
updateAddress_state :: Lens.Lens' UpdateAddress (Prelude.Maybe Prelude.Text)
updateAddress_state = Lens.lens (\UpdateAddress' {state} -> state) (\s@UpdateAddress' {} a -> s {state = a} :: UpdateAddress)

-- | The fourth line of a customer address.
updateAddress_address4 :: Lens.Lens' UpdateAddress (Prelude.Maybe Prelude.Text)
updateAddress_address4 = Lens.lens (\UpdateAddress' {address4} -> address4) (\s@UpdateAddress' {} a -> s {address4 = a} :: UpdateAddress)

-- | The county in which a customer lives.
updateAddress_county :: Lens.Lens' UpdateAddress (Prelude.Maybe Prelude.Text)
updateAddress_county = Lens.lens (\UpdateAddress' {county} -> county) (\s@UpdateAddress' {} a -> s {county = a} :: UpdateAddress)

-- | The third line of a customer address.
updateAddress_address3 :: Lens.Lens' UpdateAddress (Prelude.Maybe Prelude.Text)
updateAddress_address3 = Lens.lens (\UpdateAddress' {address3} -> address3) (\s@UpdateAddress' {} a -> s {address3 = a} :: UpdateAddress)

-- | The postal code of a customer address.
updateAddress_postalCode :: Lens.Lens' UpdateAddress (Prelude.Maybe Prelude.Text)
updateAddress_postalCode = Lens.lens (\UpdateAddress' {postalCode} -> postalCode) (\s@UpdateAddress' {} a -> s {postalCode = a} :: UpdateAddress)

-- | The country in which a customer lives.
updateAddress_country :: Lens.Lens' UpdateAddress (Prelude.Maybe Prelude.Text)
updateAddress_country = Lens.lens (\UpdateAddress' {country} -> country) (\s@UpdateAddress' {} a -> s {country = a} :: UpdateAddress)

-- | The city in which a customer lives.
updateAddress_city :: Lens.Lens' UpdateAddress (Prelude.Maybe Prelude.Text)
updateAddress_city = Lens.lens (\UpdateAddress' {city} -> city) (\s@UpdateAddress' {} a -> s {city = a} :: UpdateAddress)

-- | The first line of a customer address.
updateAddress_address1 :: Lens.Lens' UpdateAddress (Prelude.Maybe Prelude.Text)
updateAddress_address1 = Lens.lens (\UpdateAddress' {address1} -> address1) (\s@UpdateAddress' {} a -> s {address1 = a} :: UpdateAddress)

-- | The province in which a customer lives.
updateAddress_province :: Lens.Lens' UpdateAddress (Prelude.Maybe Prelude.Text)
updateAddress_province = Lens.lens (\UpdateAddress' {province} -> province) (\s@UpdateAddress' {} a -> s {province = a} :: UpdateAddress)

instance Prelude.Hashable UpdateAddress where
  hashWithSalt salt' UpdateAddress' {..} =
    salt' `Prelude.hashWithSalt` province
      `Prelude.hashWithSalt` address1
      `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` postalCode
      `Prelude.hashWithSalt` address3
      `Prelude.hashWithSalt` county
      `Prelude.hashWithSalt` address4
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` address2

instance Prelude.NFData UpdateAddress where
  rnf UpdateAddress' {..} =
    Prelude.rnf address2
      `Prelude.seq` Prelude.rnf province
      `Prelude.seq` Prelude.rnf address1
      `Prelude.seq` Prelude.rnf city
      `Prelude.seq` Prelude.rnf country
      `Prelude.seq` Prelude.rnf postalCode
      `Prelude.seq` Prelude.rnf address3
      `Prelude.seq` Prelude.rnf county
      `Prelude.seq` Prelude.rnf address4
      `Prelude.seq` Prelude.rnf state

instance Core.ToJSON UpdateAddress where
  toJSON UpdateAddress' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Address2" Core..=) Prelude.<$> address2,
            ("State" Core..=) Prelude.<$> state,
            ("Address4" Core..=) Prelude.<$> address4,
            ("County" Core..=) Prelude.<$> county,
            ("Address3" Core..=) Prelude.<$> address3,
            ("PostalCode" Core..=) Prelude.<$> postalCode,
            ("Country" Core..=) Prelude.<$> country,
            ("City" Core..=) Prelude.<$> city,
            ("Address1" Core..=) Prelude.<$> address1,
            ("Province" Core..=) Prelude.<$> province
          ]
      )
