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
-- Module      : Amazonka.LexRuntime.Types.ResponseCard
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexRuntime.Types.ResponseCard where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LexRuntime.Types.ContentType
import Amazonka.LexRuntime.Types.GenericAttachment
import qualified Amazonka.Prelude as Prelude

-- | If you configure a response card when creating your bots, Amazon Lex
-- substitutes the session attributes and slot values that are available,
-- and then returns it. The response card can also come from a Lambda
-- function ( @dialogCodeHook@ and @fulfillmentActivity@ on an intent).
--
-- /See:/ 'newResponseCard' smart constructor.
data ResponseCard = ResponseCard'
  { -- | An array of attachment objects representing options.
    genericAttachments :: Prelude.Maybe [GenericAttachment],
    -- | The version of the response card format.
    version :: Prelude.Maybe Prelude.Text,
    -- | The content type of the response.
    contentType :: Prelude.Maybe ContentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseCard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'genericAttachments', 'responseCard_genericAttachments' - An array of attachment objects representing options.
--
-- 'version', 'responseCard_version' - The version of the response card format.
--
-- 'contentType', 'responseCard_contentType' - The content type of the response.
newResponseCard ::
  ResponseCard
newResponseCard =
  ResponseCard'
    { genericAttachments = Prelude.Nothing,
      version = Prelude.Nothing,
      contentType = Prelude.Nothing
    }

-- | An array of attachment objects representing options.
responseCard_genericAttachments :: Lens.Lens' ResponseCard (Prelude.Maybe [GenericAttachment])
responseCard_genericAttachments = Lens.lens (\ResponseCard' {genericAttachments} -> genericAttachments) (\s@ResponseCard' {} a -> s {genericAttachments = a} :: ResponseCard) Prelude.. Lens.mapping Lens.coerced

-- | The version of the response card format.
responseCard_version :: Lens.Lens' ResponseCard (Prelude.Maybe Prelude.Text)
responseCard_version = Lens.lens (\ResponseCard' {version} -> version) (\s@ResponseCard' {} a -> s {version = a} :: ResponseCard)

-- | The content type of the response.
responseCard_contentType :: Lens.Lens' ResponseCard (Prelude.Maybe ContentType)
responseCard_contentType = Lens.lens (\ResponseCard' {contentType} -> contentType) (\s@ResponseCard' {} a -> s {contentType = a} :: ResponseCard)

instance Core.FromJSON ResponseCard where
  parseJSON =
    Core.withObject
      "ResponseCard"
      ( \x ->
          ResponseCard'
            Prelude.<$> ( x Core..:? "genericAttachments"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "version")
            Prelude.<*> (x Core..:? "contentType")
      )

instance Prelude.Hashable ResponseCard where
  hashWithSalt salt' ResponseCard' {..} =
    salt' `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` genericAttachments

instance Prelude.NFData ResponseCard where
  rnf ResponseCard' {..} =
    Prelude.rnf genericAttachments
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf version
