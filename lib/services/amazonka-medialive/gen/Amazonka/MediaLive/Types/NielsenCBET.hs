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
-- Module      : Amazonka.MediaLive.Types.NielsenCBET
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.NielsenCBET where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types.NielsenWatermarksCbetStepaside
import qualified Amazonka.Prelude as Prelude

-- | Nielsen CBET
--
-- /See:/ 'newNielsenCBET' smart constructor.
data NielsenCBET = NielsenCBET'
  { -- | Enter the CBET check digits to use in the watermark.
    cbetCheckDigitString :: Prelude.Text,
    -- | Determines the method of CBET insertion mode when prior encoding is
    -- detected on the same layer.
    cbetStepaside :: NielsenWatermarksCbetStepaside,
    -- | Enter the CBET Source ID (CSID) to use in the watermark
    csid :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NielsenCBET' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cbetCheckDigitString', 'nielsenCBET_cbetCheckDigitString' - Enter the CBET check digits to use in the watermark.
--
-- 'cbetStepaside', 'nielsenCBET_cbetStepaside' - Determines the method of CBET insertion mode when prior encoding is
-- detected on the same layer.
--
-- 'csid', 'nielsenCBET_csid' - Enter the CBET Source ID (CSID) to use in the watermark
newNielsenCBET ::
  -- | 'cbetCheckDigitString'
  Prelude.Text ->
  -- | 'cbetStepaside'
  NielsenWatermarksCbetStepaside ->
  -- | 'csid'
  Prelude.Text ->
  NielsenCBET
newNielsenCBET
  pCbetCheckDigitString_
  pCbetStepaside_
  pCsid_ =
    NielsenCBET'
      { cbetCheckDigitString =
          pCbetCheckDigitString_,
        cbetStepaside = pCbetStepaside_,
        csid = pCsid_
      }

-- | Enter the CBET check digits to use in the watermark.
nielsenCBET_cbetCheckDigitString :: Lens.Lens' NielsenCBET Prelude.Text
nielsenCBET_cbetCheckDigitString = Lens.lens (\NielsenCBET' {cbetCheckDigitString} -> cbetCheckDigitString) (\s@NielsenCBET' {} a -> s {cbetCheckDigitString = a} :: NielsenCBET)

-- | Determines the method of CBET insertion mode when prior encoding is
-- detected on the same layer.
nielsenCBET_cbetStepaside :: Lens.Lens' NielsenCBET NielsenWatermarksCbetStepaside
nielsenCBET_cbetStepaside = Lens.lens (\NielsenCBET' {cbetStepaside} -> cbetStepaside) (\s@NielsenCBET' {} a -> s {cbetStepaside = a} :: NielsenCBET)

-- | Enter the CBET Source ID (CSID) to use in the watermark
nielsenCBET_csid :: Lens.Lens' NielsenCBET Prelude.Text
nielsenCBET_csid = Lens.lens (\NielsenCBET' {csid} -> csid) (\s@NielsenCBET' {} a -> s {csid = a} :: NielsenCBET)

instance Core.FromJSON NielsenCBET where
  parseJSON =
    Core.withObject
      "NielsenCBET"
      ( \x ->
          NielsenCBET'
            Prelude.<$> (x Core..: "cbetCheckDigitString")
            Prelude.<*> (x Core..: "cbetStepaside")
            Prelude.<*> (x Core..: "csid")
      )

instance Prelude.Hashable NielsenCBET where
  hashWithSalt salt' NielsenCBET' {..} =
    salt' `Prelude.hashWithSalt` csid
      `Prelude.hashWithSalt` cbetStepaside
      `Prelude.hashWithSalt` cbetCheckDigitString

instance Prelude.NFData NielsenCBET where
  rnf NielsenCBET' {..} =
    Prelude.rnf cbetCheckDigitString
      `Prelude.seq` Prelude.rnf csid
      `Prelude.seq` Prelude.rnf cbetStepaside

instance Core.ToJSON NielsenCBET where
  toJSON NielsenCBET' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "cbetCheckDigitString"
                  Core..= cbetCheckDigitString
              ),
            Prelude.Just ("cbetStepaside" Core..= cbetStepaside),
            Prelude.Just ("csid" Core..= csid)
          ]
      )
