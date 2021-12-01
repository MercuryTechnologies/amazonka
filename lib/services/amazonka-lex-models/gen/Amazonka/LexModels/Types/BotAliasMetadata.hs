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
-- Module      : Amazonka.LexModels.Types.BotAliasMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.BotAliasMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LexModels.Types.ConversationLogsResponse
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a bot alias.
--
-- /See:/ 'newBotAliasMetadata' smart constructor.
data BotAliasMetadata = BotAliasMetadata'
  { -- | Checksum of the bot alias.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | The version of the Amazon Lex bot to which the alias points.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the bot to which the alias points.
    botName :: Prelude.Maybe Prelude.Text,
    -- | The date that the bot alias was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The name of the bot alias.
    name :: Prelude.Maybe Prelude.Text,
    -- | Settings that determine how Amazon Lex uses conversation logs for the
    -- alias.
    conversationLogs :: Prelude.Maybe ConversationLogsResponse,
    -- | The date that the bot alias was updated. When you create a resource, the
    -- creation date and last updated date are the same.
    lastUpdatedDate :: Prelude.Maybe Core.POSIX,
    -- | A description of the bot alias.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotAliasMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksum', 'botAliasMetadata_checksum' - Checksum of the bot alias.
--
-- 'botVersion', 'botAliasMetadata_botVersion' - The version of the Amazon Lex bot to which the alias points.
--
-- 'botName', 'botAliasMetadata_botName' - The name of the bot to which the alias points.
--
-- 'createdDate', 'botAliasMetadata_createdDate' - The date that the bot alias was created.
--
-- 'name', 'botAliasMetadata_name' - The name of the bot alias.
--
-- 'conversationLogs', 'botAliasMetadata_conversationLogs' - Settings that determine how Amazon Lex uses conversation logs for the
-- alias.
--
-- 'lastUpdatedDate', 'botAliasMetadata_lastUpdatedDate' - The date that the bot alias was updated. When you create a resource, the
-- creation date and last updated date are the same.
--
-- 'description', 'botAliasMetadata_description' - A description of the bot alias.
newBotAliasMetadata ::
  BotAliasMetadata
newBotAliasMetadata =
  BotAliasMetadata'
    { checksum = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      botName = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      name = Prelude.Nothing,
      conversationLogs = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Checksum of the bot alias.
botAliasMetadata_checksum :: Lens.Lens' BotAliasMetadata (Prelude.Maybe Prelude.Text)
botAliasMetadata_checksum = Lens.lens (\BotAliasMetadata' {checksum} -> checksum) (\s@BotAliasMetadata' {} a -> s {checksum = a} :: BotAliasMetadata)

-- | The version of the Amazon Lex bot to which the alias points.
botAliasMetadata_botVersion :: Lens.Lens' BotAliasMetadata (Prelude.Maybe Prelude.Text)
botAliasMetadata_botVersion = Lens.lens (\BotAliasMetadata' {botVersion} -> botVersion) (\s@BotAliasMetadata' {} a -> s {botVersion = a} :: BotAliasMetadata)

-- | The name of the bot to which the alias points.
botAliasMetadata_botName :: Lens.Lens' BotAliasMetadata (Prelude.Maybe Prelude.Text)
botAliasMetadata_botName = Lens.lens (\BotAliasMetadata' {botName} -> botName) (\s@BotAliasMetadata' {} a -> s {botName = a} :: BotAliasMetadata)

-- | The date that the bot alias was created.
botAliasMetadata_createdDate :: Lens.Lens' BotAliasMetadata (Prelude.Maybe Prelude.UTCTime)
botAliasMetadata_createdDate = Lens.lens (\BotAliasMetadata' {createdDate} -> createdDate) (\s@BotAliasMetadata' {} a -> s {createdDate = a} :: BotAliasMetadata) Prelude.. Lens.mapping Core._Time

-- | The name of the bot alias.
botAliasMetadata_name :: Lens.Lens' BotAliasMetadata (Prelude.Maybe Prelude.Text)
botAliasMetadata_name = Lens.lens (\BotAliasMetadata' {name} -> name) (\s@BotAliasMetadata' {} a -> s {name = a} :: BotAliasMetadata)

-- | Settings that determine how Amazon Lex uses conversation logs for the
-- alias.
botAliasMetadata_conversationLogs :: Lens.Lens' BotAliasMetadata (Prelude.Maybe ConversationLogsResponse)
botAliasMetadata_conversationLogs = Lens.lens (\BotAliasMetadata' {conversationLogs} -> conversationLogs) (\s@BotAliasMetadata' {} a -> s {conversationLogs = a} :: BotAliasMetadata)

-- | The date that the bot alias was updated. When you create a resource, the
-- creation date and last updated date are the same.
botAliasMetadata_lastUpdatedDate :: Lens.Lens' BotAliasMetadata (Prelude.Maybe Prelude.UTCTime)
botAliasMetadata_lastUpdatedDate = Lens.lens (\BotAliasMetadata' {lastUpdatedDate} -> lastUpdatedDate) (\s@BotAliasMetadata' {} a -> s {lastUpdatedDate = a} :: BotAliasMetadata) Prelude.. Lens.mapping Core._Time

-- | A description of the bot alias.
botAliasMetadata_description :: Lens.Lens' BotAliasMetadata (Prelude.Maybe Prelude.Text)
botAliasMetadata_description = Lens.lens (\BotAliasMetadata' {description} -> description) (\s@BotAliasMetadata' {} a -> s {description = a} :: BotAliasMetadata)

instance Core.FromJSON BotAliasMetadata where
  parseJSON =
    Core.withObject
      "BotAliasMetadata"
      ( \x ->
          BotAliasMetadata'
            Prelude.<$> (x Core..:? "checksum")
            Prelude.<*> (x Core..:? "botVersion")
            Prelude.<*> (x Core..:? "botName")
            Prelude.<*> (x Core..:? "createdDate")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "conversationLogs")
            Prelude.<*> (x Core..:? "lastUpdatedDate")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable BotAliasMetadata where
  hashWithSalt salt' BotAliasMetadata' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdatedDate
      `Prelude.hashWithSalt` conversationLogs
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` botName
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` checksum

instance Prelude.NFData BotAliasMetadata where
  rnf BotAliasMetadata' {..} =
    Prelude.rnf checksum
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf conversationLogs
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf botName
      `Prelude.seq` Prelude.rnf botVersion
