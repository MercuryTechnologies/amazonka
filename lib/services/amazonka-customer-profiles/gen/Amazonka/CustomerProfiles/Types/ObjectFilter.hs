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
-- Module      : Amazonka.CustomerProfiles.Types.ObjectFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.ObjectFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The filter applied to ListProfileObjects response to include profile
-- objects with the specified index values. This filter is only supported
-- for ObjectTypeName _asset and _case.
--
-- /See:/ 'newObjectFilter' smart constructor.
data ObjectFilter = ObjectFilter'
  { -- | A searchable identifier of a standard profile object. The predefined
    -- keys you can use to search for _asset include: _assetId, _assetName,
    -- _serialNumber. The predefined keys you can use to search for _case
    -- include: _caseId.
    keyName :: Prelude.Text,
    -- | A list of key values.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ObjectFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyName', 'objectFilter_keyName' - A searchable identifier of a standard profile object. The predefined
-- keys you can use to search for _asset include: _assetId, _assetName,
-- _serialNumber. The predefined keys you can use to search for _case
-- include: _caseId.
--
-- 'values', 'objectFilter_values' - A list of key values.
newObjectFilter ::
  -- | 'keyName'
  Prelude.Text ->
  ObjectFilter
newObjectFilter pKeyName_ =
  ObjectFilter'
    { keyName = pKeyName_,
      values = Prelude.mempty
    }

-- | A searchable identifier of a standard profile object. The predefined
-- keys you can use to search for _asset include: _assetId, _assetName,
-- _serialNumber. The predefined keys you can use to search for _case
-- include: _caseId.
objectFilter_keyName :: Lens.Lens' ObjectFilter Prelude.Text
objectFilter_keyName = Lens.lens (\ObjectFilter' {keyName} -> keyName) (\s@ObjectFilter' {} a -> s {keyName = a} :: ObjectFilter)

-- | A list of key values.
objectFilter_values :: Lens.Lens' ObjectFilter [Prelude.Text]
objectFilter_values = Lens.lens (\ObjectFilter' {values} -> values) (\s@ObjectFilter' {} a -> s {values = a} :: ObjectFilter) Prelude.. Lens.coerced

instance Prelude.Hashable ObjectFilter where
  hashWithSalt salt' ObjectFilter' {..} =
    salt' `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` keyName

instance Prelude.NFData ObjectFilter where
  rnf ObjectFilter' {..} =
    Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf values

instance Core.ToJSON ObjectFilter where
  toJSON ObjectFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("KeyName" Core..= keyName),
            Prelude.Just ("Values" Core..= values)
          ]
      )
